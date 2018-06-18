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
module Cardano.Faucet.Types (
   FaucetConfig(..)
 , HasFaucetConfig(..)
 , FaucetEnv(..)
 , HasFaucetEnv(..)
 , SourceWalletConfig(..)
 , srcSpendingPassword
 , cfgToPaymentSource
 , incWithDrawn
 , decrWithDrawn
 , setWalletBalance
 , WithDrawlRequest(..), wAddress
 , WithDrawlResult(..)
 , DepositRequest(..), dWalletId, dAmount
 , SourceWallet(..)
 , InitializedWallet(..), walletBalance, walletConfig
 , CreatedWallet(..)
 , InitFaucetError(..)
 , DepositResult(..)
 , M, runM
 , MonadFaucet
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (Exception)
import           Control.Lens hiding ((.=))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:),
                             (.:?), (.=))
import           Data.Int (Int64)
import           Data.Text (Text)
-- import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Servant (ServantErr)
import           System.Metrics (Store)
import           System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import           System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as Gauge
import           System.Remote.Monitoring.Statsd (StatsdOptions (..))
import           System.Wlog (CanLog, HasLoggerName, LoggerName (..), LoggerNameBox (..),
                              WithLogger, launchFromFile)

import           Cardano.Wallet.API.V1.Types (AccountIndex, PaymentSource (..), Transaction,
                                              V1 (..), WalletId (..))
import           Cardano.Wallet.Client (ClientError (..), WalletClient (..))
import           Pos.Core (Address (..), Coin (..))
import           Pos.Util.BackupPhrase (BackupPhrase (..))


--------------------------------------------------------------------------------
data WithDrawlRequest = WithDrawlRequest {
    _wAddress :: !(V1 Address)
  } deriving (Show, Typeable, Generic)

makeLenses ''WithDrawlRequest

instance FromJSON WithDrawlRequest where
  parseJSON = withObject "WithDrawlRequest" $ \v -> WithDrawlRequest
    <$> v .: "address"

instance ToJSON WithDrawlRequest where
    toJSON (WithDrawlRequest w) =
        object [ "address" .= w ]

data WithDrawlResult =
    WithdrawlError ClientError
  | WithdrawlSuccess Transaction
  deriving (Show, Typeable, Generic)

instance ToJSON WithDrawlResult where
    toJSON (WithdrawlSuccess txn) =
        object ["success" .= txn]
    toJSON (WithdrawlError err) =
        object ["error" .= show err]


--------------------------------------------------------------------------------
data DepositRequest = DepositRequest {
    _dWalletId :: Text
  , _dAmount   :: Coin
  } deriving (Show, Typeable, Generic)

makeLenses ''DepositRequest

instance FromJSON DepositRequest where
  parseJSON = withObject "DepositRequest" $ \v -> DepositRequest
    <$> v .: "wallet"
    <*> (Coin <$> v .: "amount")

data DepositResult = DepositResult
  deriving (Show, Typeable, Generic)

instance ToJSON DepositResult

--------------------------------------------------------------------------------
newtype FaucetStatsdOpts = FaucetStatsdOpts StatsdOptions deriving (Generic)

makeWrapped ''FaucetStatsdOpts

instance FromJSON FaucetStatsdOpts where
    parseJSON = fmap FaucetStatsdOpts . (withObject "StatsdOptions" $ \v ->
        StatsdOptions
          <$> v .: "host"
          <*> v .: "port"
          <*> v .: "flush-interval"
          <*> pure False
          <*> pure "faucet"
          <*> pure "")

--------------------------------------------------------------------------------
data SourceWalletConfig = SourceWalletConfig {
    _srcWalletId         :: !WalletId
  , _srcAccountIndex     :: !AccountIndex
  , _srcSpendingPassword :: !(Maybe Text)
  } deriving (Generic, Show)

srcSpendingPassword :: Lens' SourceWalletConfig (Maybe Text)
srcSpendingPassword f = \(SourceWalletConfig w a p) ->
    SourceWalletConfig w a <$> f p

instance FromJSON SourceWalletConfig where
    parseJSON = withObject "SourceWalletConfig" $ \v -> SourceWalletConfig
      <$> v .: "wallet-id"
      <*> v .: "account-index"
      <*> v .:? "spending-password"

cfgToPaymentSource :: SourceWalletConfig -> PaymentSource
cfgToPaymentSource (SourceWalletConfig wId aIdx _) = PaymentSource wId aIdx

--------------------------------------------------------------------------------
newtype PaymentCenter = PaymentCenter Int deriving (Generic, Show, Eq, Ord)

makeWrapped ''PaymentCenter

instance FromJSON PaymentCenter

--------------------------------------------------------------------------------
newtype PaymentVariation = PaymentVariation Float deriving (Generic, Show, Eq, Ord)

makeWrapped ''PaymentVariation

instance FromJSON PaymentVariation

--------------------------------------------------------------------------------
data SourceWallet = Generate !FilePath
                  | Provided !FilePath

instance FromJSON SourceWallet where
    parseJSON = withObject "SourceWallet" $ \v ->
        (Generate <$> v .: "generate-to") <|> (Provided <$> v .: "read-from")

data InitializedWallet = InitializedWallet {
    _walletConfig   :: !SourceWalletConfig
  , _walletBalance :: !Int64
  } deriving (Show, Generic)

makeLenses ''InitializedWallet
--------------------------------------------------------------------------------
data FaucetConfig = FaucetConfig {
    _fcWalletApiHost    :: !String
  , _fcWalletApiPort    :: !Int
  , _fcPort             :: !Int
  , _fcPaymentAmount    :: !PaymentCenter
  , _fcPaymentVariation :: !PaymentVariation
  , _fcStatsdOpts       :: !FaucetStatsdOpts
  , _fcSourceWallet     :: !SourceWallet
  , _fcLoggerConfigFile :: !FilePath
  , _fcPubCertFile      :: !FilePath
  , _fcPrivKeyFile      :: !FilePath
  }

makeClassy ''FaucetConfig

instance FromJSON FaucetConfig where
    parseJSON = withObject "FaucetConfig" $ \v ->
        FaucetConfig
          <$> v .: "wallet-host"
          <*> v .: "wallet-port"
          <*> v .: "port"
          <*> v .: "payment-amount"
          <*> v .: "payment-variation"
          <*> v .: "statsd"
          <*> v .: "source-wallet"
          <*> v .: "logging-config"
          <*> v .: "public-certificate"
          <*> v .: "private-key"

data CreatedWallet = CreatedWallet {
    _createdWalletId :: WalletId
  , _createdPhrase   :: BackupPhrase
  , _createdAcctIdx  :: AccountIndex
  , _createdAddress  :: Address
  } deriving (Show, Generic)

--------------------------------------------------------------------------------
instance ToJSON CreatedWallet where
    toJSON (CreatedWallet wId phrase acctIdx addr) =
        object [ "wallet-id" .= wId
               , "recovery-words" .= (Text.unwords $ bpToList phrase)
               , "account-index" .= acctIdx
               , "address" .= addr
               ]
--------------------------------------------------------------------------------
data InitFaucetError =
    SourceWalletParseError String
  | CouldntReadSyncState ClientError
  | WalletCreationError ClientError
  | CouldntReadBalance ClientError
  | NoWalletAccounts WalletId
  | MultipleWalletAccounts WalletId
  | BadAddress WalletId AccountIndex
  deriving (Typeable, Show)

instance Exception InitFaucetError

--------------------------------------------------------------------------------
data FaucetEnv = FaucetEnv {
    _feWithdrawn     :: !Counter
  , _feNumWithdrawn  :: !Counter
  , _feWalletBalance :: !Gauge
  , _feStore         :: !Store
  , _feSourceWallet  :: !SourceWalletConfig
  , _feFaucetConfig  :: !FaucetConfig
  , _feWalletClient  :: !(WalletClient IO)
  }

makeClassy ''FaucetEnv


incWithDrawn :: (MonadReader e m, HasFaucetEnv e, MonadIO m) => Coin -> m ()
incWithDrawn (Coin (fromIntegral -> c)) = do
  wd <- view feWithdrawn
  wc <- view feNumWithdrawn
  bal <- view feWalletBalance
  liftIO $ do
    Counter.add wd c
    Counter.inc wc
    Gauge.add bal c

decrWithDrawn :: (MonadReader e m, HasFaucetEnv e, MonadIO m) => Coin -> m ()
decrWithDrawn (Coin (fromIntegral -> c)) = do
  -- wd <- view feWithdrawn
  -- wc <- view feNumWithdrawn
  bal <- view feWalletBalance
  liftIO $ do
    -- Counter.subtract wd c
    -- Counter.inc wc
    Gauge.subtract bal c

setWalletBalance :: (MonadReader e m, HasFaucetEnv e, MonadIO m) => Coin -> m ()
setWalletBalance (Coin (fromIntegral -> c)) = do
  bal <- view feWalletBalance
  liftIO $ Gauge.set bal c

--------------------------------------------------------------------------------
newtype M a = M { unM :: ReaderT FaucetEnv (ExceptT ServantErr (LoggerNameBox IO)) a }
  deriving (Functor, Applicative, Monad, MonadReader FaucetEnv, CanLog, HasLoggerName, MonadIO)

runM :: FaucetEnv -> M a -> IO (Either ServantErr a)
runM c = launchFromFile (c ^. feFaucetConfig . fcLoggerConfigFile) (LoggerName "faucet")
       . runExceptT
       . flip runReaderT c
       . unM

type MonadFaucet c m = (MonadIO m, MonadReader c m, HasFaucetEnv c, WithLogger m, HasLoggerName m)
