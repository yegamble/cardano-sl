-- | Types used for communication about transactions.

module Pos.Txp.Network.Types
       ( TxMsgContents (..)
       ) where

import           Formatting.Buildable (Buildable (build))

import           Formatting (bprint, (%))
import           Universum

import           Pos.Core.Txp (TxAux (..), txaF)

-- | Data message. Can be used to send one transaction per message.
-- Transaction is sent with auxilary data.
newtype TxMsgContents = TxMsgContents
    { getTxMsgContents :: TxAux
    } deriving (Generic, Show, Eq)

instance Buildable TxMsgContents where
    build (TxMsgContents txAux) =
        bprint ("TxMsgContents { txAux ="%txaF%", .. }") txAux
