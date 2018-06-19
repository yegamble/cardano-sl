module Cardano.Wallet.WalletLayer.Transactions where

import           Universum

import qualified Pos.Core as Core
import Cardano.Wallet.Kernel.DB.BlockMeta
import Cardano.Wallet.Kernel.DB.InDb
import qualified Data.Map as M
import Pos.Core as Core
import Cardano.Wallet.API.V1.Types
import Cardano.Wallet.Kernel.DB.TxMeta
import Cardano.Wallet.Kernel.DB.Spec

getTransactions
    :: MonadIO m
    => MetaDBHandle
    -> Maybe WalletId
    -> Maybe AccountIndex
    -> Maybe (V1 Core.Address)
    -> m [Transaction]
getTransactions MetaDBHandle{..} _ _ _ = liftIO $ do
  meta <- getTxMetas (Offset 0) (Limit 100) Nothing
  pending <- empty -- error "TODO"
  blockMeta <- return (mempty :: BlockMeta) -- TODO: find blockMeta
  k <- return (2000 :: Word) -- TODO: retrieve this constant
  let
      mp :: Map Core.TxId Core.SlotId
      mp = _fromDb $ _blockMetaSlotId blockMeta

      pendingTxs :: Map Core.TxId Core.TxAux
      pendingTxs = _fromDb $ _pendingTransactions pending

      metaToTx :: TxMeta -> Transaction
      metaToTx TxMeta{..} =
        Transaction txId confirmations amount inputs outputs txType direction time status
          where
            -- First find the fields from MetaData.
            txId = V1 _txMetaId
            amount = V1 _txMetaAmount
            inputs = do   -- TODO: make this a function?
              (adr, coin) <- _txMetaInputs
              return $ PaymentDistribution (V1 adr) (V1 coin)
            outputs = do
              (adr, coin) <- _txMetaOutputs
              return $ PaymentDistribution (V1 adr) (V1 coin)
            txType = if _txMetaIsLocal then LocalTransaction else ForeignTransaction
            direction = if _txMetaIsOutgoing then OutgoingTransaction else IncomingTransaction
            time = V1 _txMetaCreationAt

            -- Now find the fields from BlockMeta.
            mSlot :: Maybe Core.SlotId
            mSlot = mp M.!? _txMetaId
            inPending = M.member _txMetaId pendingTxs
            (status, confirmations) = case inPending of
              True  -> (Applying, 0)
              False -> case mSlot of
                Nothing     -> (WontApply, 0)
                Just (SlotId (EpochIndex w64) (UnsafeLocalSlotIndex w16)) ->
                  case (k >= 0) of              -- TODO: fix
                    True -> (InNewestBlocks, 0) -- TODO: fix
                    False -> (Persisted, 0)     -- TODO: fix

  return $ map metaToTx meta -- TODO: we need any filter here?
