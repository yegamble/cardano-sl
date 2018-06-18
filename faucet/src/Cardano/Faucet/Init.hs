{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.Faucet.Init (initEnv) where

import Control.Concurrent (threadDelay)
import           Control.Exception (throw)
import           Control.Lens hiding ((.=))
import           Control.Monad.Except
import           Crypto.Random.Entropy (getEntropy)
import           Data.Aeson (eitherDecode)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text
import           Data.Text.Lens (packed)
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (ClientParams (..), credentialLoadX509FromMemory, defaultParamsClient,
                              onCertificateRequest, onServerCertificate, supportedCiphers)
import           Network.TLS.Extra.Cipher (ciphersuite_strong)
import           Servant.Client.Core (BaseUrl (..), Scheme (..))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
import           System.Metrics (Store, createCounter, createGauge)
import qualified System.Metrics.Gauge as Gauge
import           System.Wlog (CanLog, HasLoggerName, LoggerNameBox (..), logError, logInfo,
                              withSublogger)

import           Cardano.Wallet.API.V1.Types (Account (..), AssuranceLevel (NormalAssurance),
                                              NewWallet (..), NodeInfo (..), PaymentSource (..),
                                              SyncPercentage, V1 (..), Wallet (..),
                                              WalletAddress (..), WalletOperation (CreateWallet),
                                              mkSyncPercentage, unV1)
import           Cardano.Wallet.Client (ClientError (..), WalletClient (..), WalletResponse (..),
                                        liftClient)
import           Cardano.Wallet.Client.Http (mkHttpClient)
import           Pos.Core (Address (..), Coin (..))
import           Pos.Util.BackupPhrase (BackupPhrase (..))
import           Pos.Util.Mnemonics (toMnemonic)

import           Cardano.Faucet.Types


--------------------------------------------------------------------------------
readSourceWalletConfig :: FilePath -> IO (Either String SourceWalletConfig)
readSourceWalletConfig = fmap eitherDecode . BSL.readFile

--------------------------------------------------------------------------------
generateBackupPhrase :: IO BackupPhrase
generateBackupPhrase = do
    -- The size 16 should give us 12-words mnemonic after BIP-39 encoding.
    genMnemonic <- getEntropy 16
    let newMnemonic = either (error . show) id $ toMnemonic genMnemonic
    return $ mkBackupPhrase12 $ Text.words newMnemonic
  where
    mkBackupPhrase12 :: [Text] -> BackupPhrase
    mkBackupPhrase12 ls
        | length ls == 12 = BackupPhrase ls
        | otherwise = error "Invalid number of words in backup phrase! Expected 12 words."

--------------------------------------------------------------------------------
completelySynced :: SyncPercentage
completelySynced = mkSyncPercentage 100

getSyncState
    :: (HasLoggerName m, MonadIO m)
    => WalletClient m
    -> m (Either ClientError SyncPercentage)
getSyncState client = do
    r <- getNodeInfo client
    return (nfoSyncProgress . wrData <$> r)

--------------------------------------------------------------------------------
createWallet
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => WalletClient m
    -> m (Either InitFaucetError (SourceWalletConfig, BackupPhrase, Address))
createWallet client = do
    sync <- getSyncState client
    case sync of
        Left err -> do
            logError $ "Error getting sync state: " <> (Text.pack $ show err)
            return . Left $ CouldntReadBalance err
        Right ss | ss == completelySynced -> do
                       logInfo "Node fully synced, creating wallet"
                       mkWallet
                 | otherwise -> do
                       logInfo $ "Node not fully synced: " <> (Text.pack $ show ss)
                       liftIO $ threadDelay 5000000
                       createWallet client
    where
        mkWallet = do
          phrase <- liftIO generateBackupPhrase
          let w = NewWallet (V1 phrase) Nothing NormalAssurance "Faucet-Wallet" CreateWallet
          runExceptT $ do
              wId <- walId <$> (runClient WalletCreationError $ postWallet client w)
              logInfo $ "Created wallet with ID: " <> (Text.pack $ show wId)
              accounts <- runClient WalletCreationError $
                            getAccountIndexPaged
                              client
                              wId
                              Nothing
                              Nothing
              case accounts of
                  [a] -> do
                      let aIdx = accIndex a
                      logInfo $ "Found a single account for wallet with ID: "
                                <> (Text.pack $ show wId)
                                <> " account index: " <> (Text.pack $ show aIdx)
                      case accAddresses a of
                          [addr] -> do
                              logInfo $ "Found a single account for wallet with ID: "
                                        <> (Text.pack $ show wId)
                                        <> " account index: " <> (Text.pack $ show aIdx)
                              return (SourceWalletConfig wId aIdx Nothing, phrase, unV1 $ addrId addr)
                          _ -> do
                              logInfo $ "Didn't find an address for wallet with ID: "
                                        <> (Text.pack $ show wId)
                                        <> " account index: " <> (Text.pack $ show aIdx)
                              throwError $ BadAddress wId aIdx
                  [] -> do
                      logError $ "No accounts found for wallet with ID: " <> (Text.pack $ show wId)
                      throwError $ NoWalletAccounts wId
                  _ -> do
                      logError $ "Multiple accounts found for wallet with ID: " <> (Text.pack $ show wId)
                      throwError $ MultipleWalletAccounts wId
        runClient err m = ExceptT $ (fmap (first err)) $ fmap (fmap wrData) m

--------------------------------------------------------------------------------
writeCreatedWalletInfo :: FilePath -> CreatedWallet -> IO ()
writeCreatedWalletInfo fp cw = do
    let theDir = takeDirectory fp
    createDirectoryIfMissing True theDir
    Text.writeFile fp $ encodeToLazyText cw

--------------------------------------------------------------------------------
readWalletBalance
    :: (HasLoggerName m, MonadIO m)
    => WalletClient m
    -> PaymentSource
    -> m (Either InitFaucetError Int64)
readWalletBalance client (psWalletId -> wId) = do
    r <- getWallet client wId
    return $ first CouldntReadBalance $ fmap (fromIntegral . getCoin . unV1 . walBalance . wrData) $ r

makeInitializedWallet
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => FaucetConfig
    -> WalletClient m
    -> m (Either InitFaucetError InitializedWallet)
makeInitializedWallet fc client = withSublogger "makeInitializedWallet" $ do
    case (fc ^. fcSourceWallet) of
        Provided fp -> do
            srcCfg <- liftIO $ readSourceWalletConfig fp
            case srcCfg of
                Left e -> return $ Left $ SourceWalletParseError e
                Right wc -> do
                    let ps = cfgToPaymentSource wc
                    fmap (InitializedWallet wc) <$> readWalletBalance client ps
        Generate fp -> do
            resp <- createWallet client
            forM resp $ \(swc@(SourceWalletConfig wallet accIdx _), phrase, addr) -> do
                    let iw = InitializedWallet swc 0
                        createdWallet = CreatedWallet wallet phrase accIdx addr
                    liftIO $ writeCreatedWalletInfo fp createdWallet
                    return iw

initEnv :: FaucetConfig -> Store -> LoggerNameBox IO FaucetEnv
initEnv fc store = withSublogger "init" $ do
    walletBalanceGauge <- liftIO $ createGauge "wallet-balance" store
    feConstruct <- liftIO $ FaucetEnv
      <$> createCounter "total-withdrawn" store
      <*> createCounter "num-withdrawals" store
      <*> pure walletBalanceGauge
    manager <- liftIO $ createManager fc
    let url = BaseUrl Https (fc ^. fcWalletApiHost) (fc ^. fcWalletApiPort) ""
        client = mkHttpClient url manager
    initialWallet <- makeInitializedWallet fc (liftClient client)
    case initialWallet of
        Left err -> throw err
        Right iw -> do
          liftIO $ Gauge.set walletBalanceGauge (iw ^. walletBalance)
          return $ feConstruct
                      store
                      (iw ^. walletConfig)
                      fc
                      client

createManager :: FaucetConfig -> IO Manager
createManager fc = do
    pubCert <- BS.readFile (fc ^. fcPubCertFile)
    privKey <- BS.readFile (fc ^. fcPrivKeyFile)
    case credentialLoadX509FromMemory pubCert privKey of
        Left problem -> error $ "Unable to load credentials: " <> (problem ^. packed)
        Right credential ->
            let hooks = def {
                            onCertificateRequest = \_ -> return $ Just credential,
                            onServerCertificate  = \_ _ _ _ -> return []
                        }
                clientParams = (defaultParamsClient "localhost" "") {
                                   clientHooks = hooks,
                                   clientSupported = def {
                                       supportedCiphers = ciphersuite_strong
                                   }
                               }
                tlsSettings = TLSSettings clientParams
            in
            newManager $ mkManagerSettings tlsSettings Nothing
