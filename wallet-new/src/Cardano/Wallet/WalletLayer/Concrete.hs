module Cardano.Wallet.WalletLayer.Concrete where

import           Universum

import qualified Pos.Core as Core
import           Pos.Core as Core
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Kernel.DB.TxMeta
import           Cardano.Wallet.Kernel hiding (WalletId)
import qualified Cardano.Wallet.Kernel.Actions as Actions
import           Cardano.Wallet.WalletLayer.Types (PassiveWalletLayer (..))
import           Cardano.Wallet.Kernel.DB.AcidState (Snapshot (..))
import           Pos.Block.Types (Blund)
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId)
-- import qualified Data.Text.Encoding as T -- temporarily

--(Actions.WalletAction b -> m ())
-- | TODO(ks): Currently not implemented!
mkPassiveWalletLayer :: MonadIO m => PassiveWallet -> (Actions.WalletAction Blund -> m ()) -> PassiveWalletLayer m
mkPassiveWalletLayer _wallet invoke =
    PassiveWalletLayer
        { _pwlCreateWallet   = error "Not implemented!"
        , _pwlGetWalletIds   = error "Not implemented!"
        , _pwlGetWallet      = error "Not implemented!"
        , _pwlUpdateWallet   = error "Not implemented!"
        , _pwlDeleteWallet   = error "Not implemented!"

        , _pwlCreateAccount  = error "Not implemented!"
        , _pwlGetAccounts    = getAccounts _wallet Nothing
        , _pwlGetAccount     = error "Not implemented!"
        , _pwlUpdateAccount  = error "Not implemented!"
        , _pwlDeleteAccount  = error "Not implemented!"

        , _pwlGetAddresses   = error "Not implemented!"
        , _pwlGetTransactions = getTransactions _wallet Nothing

        , _pwlApplyBlocks    = invoke . Actions.ApplyBlocks
        , _pwlRollbackBlocks = invoke . Actions.RollbackBlocks
        }

-- TODO: This should be defined in Kernel.
readSnapshot :: PassiveWallet -> IO Snapshot
readSnapshot _ = error "TODO" -- getWalletSnapshot

-- | Read a Snapshot if it`s not there.
readSnapshotMaybe :: PassiveWallet -> Maybe Snapshot -> IO Snapshot
readSnapshotMaybe pwallet mbShnapshot =
    case mbShnapshot of
        Just snapshot -> return snapshot
        Nothing -> readSnapshot pwallet

getAccounts ::  MonadIO m
            => PassiveWallet
            -> Maybe Snapshot
            -> WalletId
            -> m [Account]
getAccounts pwallet mbShnapshot _ = liftIO $ do
    _ <- readSnapshotMaybe pwallet mbShnapshot
    return $ error "TODO"

getTransactions :: MonadIO m
                => PassiveWallet
                -> Maybe Snapshot
                -> Maybe WalletId
                -> Maybe AccountIndex
                -> Maybe (V1 Core.Address)
                -> m [Transaction]
getTransactions pwallet mbShnapshot mbWalletId mbAccIdx _ = liftIO $ do
    snapshot <- readSnapshotMaybe pwallet mbShnapshot
    case mbWalletId of
        Just walletId ->  do
            accIds <- case mbAccIdx of
                Just accIdx -> return [accIdx]
                Nothing     -> do
                   accountIds <- getAccounts pwallet (Just snapshot) walletId
                   return $ map accIndex accountIds

            meta <- getTxMetasByAccounts (pwallet ^. walletMeta) accIds (Offset 0) (Limit 100) Nothing
            k <- return (2000 :: Word) -- TODO: retrieve this constant
            let
                toPayDistr :: (Address, Coin) -> PaymentDistribution
                toPayDistr (addr, c) = PaymentDistribution (V1 addr) (V1 c)

                metaToTx :: MonadIO m => TxMeta -> m Transaction
                metaToTx TxMeta{..} = liftIO $ do
                  let
                      mbHdAccountId = toHdAccountId walletId _txMetaAccountId
                  case mbHdAccountId of
                      Nothing -> error "TODO" -- This means a wrong WalletId was given
                      Just hdAccountId -> do

                          -- TODO: the acid state query needs rework
                          (mSlot, inPending) <- accountTxInfo pwallet hdAccountId _txMetaId

                          let
                            (status, confirmations) = case inPending of
                              True  -> (Applying, 0)
                              False -> case mSlot of
                                Nothing     -> (WontApply, 0)
                                Just (SlotId (EpochIndex w64) (UnsafeLocalSlotIndex w16)) ->
                                  case (k >= 0) of              -- TODO: fix
                                    True -> (InNewestBlocks, fromIntegral w64) -- TODO: fix
                                    False -> (Persisted, fromIntegral w16)     -- TODO: fix
                          return $ Transaction {
                              txId = V1 _txMetaId,
                              txConfirmations = confirmations,
                              txAmount = V1 _txMetaAmount,
                              txInputs = toPayDistr <$> _txMetaInputs,
                              txOutputs = toPayDistr <$> _txMetaOutputs,
                              txType = if _txMetaIsLocal then LocalTransaction else ForeignTransaction,
                              txDirection = if _txMetaIsOutgoing then OutgoingTransaction else IncomingTransaction,
                              txCreationTime = V1 _txMetaCreationAt,
                              txStatus = status
                          }
            mapM metaToTx meta
        _ ->
            throwM MissingRequiredParams
                { requiredParams = pure ("wallet_id", "WalletId")
                }


-- We should avoid dublicate code with Renzo for this.
toHdAccountId :: WalletId -> AccountIndex -> Maybe HdAccountId
toHdAccountId _ _ = error "TODO"
{-}
    hsh <- cIdWalToHashPublicKey walletId
    mbHdRootId <- fmap (HdRootId . InDb) hsh
    return $ HdAccountId hdRootId accountIndex
    where
--      cIdWalToHashPublicKey :: WalletId -> Maybe (AbstractHash Blake2b_224 PublicKey)
        cIdWalToHashPublicKey (WalletId t0) = do
            bs0 <- decodeBase16 (T.encodeUtf8 t0)
            dig <- digestFromByteString bs0
            pure (AbstractHash dig)
-}
