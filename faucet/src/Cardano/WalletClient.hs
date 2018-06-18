{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.WalletClient (
    withdraw
  ) where

import           Cardano.Wallet.API.V1.Types (Payment (..), PaymentDistribution (..), V1 (..))
import           Cardano.Wallet.Client (Resp, Transaction, WalletClient (..), liftClient)
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Crypto.Hash (Blake2b_256, Digest)
import qualified Crypto.Hash as CryptoHash
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text.Strict.Lens (utf8)
import           Pos.Core (Address (..), Coin (..))
import           Pos.Crypto.Signing (PassPhrase)
import           System.Random

import           Cardano.Faucet.Types

randomAmount :: (MonadFaucet c m) => m (V1 Coin)
randomAmount = do
    amt <- fromIntegral <$> view (feFaucetConfig . fcPaymentAmount . _Wrapped')
    var <- view (feFaucetConfig . fcPaymentVariation . _Wrapped')
    f <- liftIO $ randomRIO ((-1), 1)
    return $ V1 $ Coin $ round (amt + (var * f))

withdraw :: (MonadFaucet c m) => V1 Address -> Resp m Transaction
withdraw addr = do
    paymentSource <- view (feSourceWallet . to cfgToPaymentSource)
    spendingPassword <- view (feSourceWallet . srcSpendingPassword)
    coin <- randomAmount
    client <- liftClient <$> view feWalletClient
    let paymentDist = (PaymentDistribution addr coin :| [])
        sp =  spendingPassword <&> view (re utf8 . to hashPwd . to V1)
        payment = Payment paymentSource paymentDist Nothing sp
    postTransaction client payment


hashPwd :: ByteString -> PassPhrase
hashPwd  bs =
    let blake = CryptoHash.hash bs :: Digest (Blake2b_256)
    in BA.convert blake
