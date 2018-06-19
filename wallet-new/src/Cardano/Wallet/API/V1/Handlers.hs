module Cardano.Wallet.API.V1.Handlers (handlers) where

import           Universum

import           Servant

import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Cardano.Wallet.WalletLayer.Types
import qualified Data.IxSet.Typed as IxSet
import           Mockable

-- TODO: make this polymorphic on Production, with something like UnliftIO.
handlers :: ActiveWalletLayer Production -> Server V1.API
handlers w = addresses :<|> wallets :<|> accounts :<|> transactions :<|> settings :<|> info
  where
    pw = walletPassiveLayer w

    addresses = todo
    wallets = todo
    accounts = todo
    transactions = (todo :<|> getTransactionsHistory :<|> todo)
    settings = todo
    info = todo

    todo = error "TODO"

    getTransactionsHistory mwalletId mAccIdx mAddr requestParams fops sops =
      liftIO $ runProduction ret
        where
          txs = getTransactions pw mwalletId mAccIdx mAddr
          ret = respondWith requestParams fops sops (IxSet.fromList <$> txs)
