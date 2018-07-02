{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.Faucet (
    FaucetAPI
  , faucetServer
  , faucetServerAPI
  , module Cardano.Faucet.Types
  , module Cardano.Faucet.Init
  ) where

import           Control.Lens
import Control.Monad (unless, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Lens (packedChars)
import           Data.Monoid ((<>))
import           Data.Tagged (retag)
import           Data.Text.Lens
import           Servant
import           System.Wlog (LoggerName (..), logError, logInfo, withSublogger)

import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.V1.Types (txAmount, unV1)
import qualified Cardano.WalletClient as Client

import           Cardano.Faucet.Init
import           Cardano.Faucet.Metrics
import           Cardano.Faucet.Types
import           Cardano.Faucet.Types.Recaptcha

-- | Top level type of the faucet API
type FaucetAPI = "api" :> "withdraw" :> Summary "Requests some ADA from the faucet"
                                     :> ReqBody '[JSON] WithdrawlRequest :> Post '[JSON] WithdrawlResult
                :<|> "withdraw" :> Summary "Requests ADA from the faucet via recaptcha enabled form"
                                :> ReqBody '[FormUrlEncoded] WithdrawlFormRequest :> Post '[JSON] WithdrawlResult
                :<|> Raw
         -- :<|> "_deposit" :> ReqBody '[JSON] DepositRequest :> Post '[JSON] DepositResult

faucetServerAPI :: Proxy FaucetAPI
faucetServerAPI = Proxy

formWithdraw :: (MonadFaucet c m) => WithdrawlFormRequest -> m WithdrawlResult
formWithdraw wfr = withSublogger (LoggerName "formWithdraw") $ do
    mCaptchaSecret <- view (feFaucetConfig . fcRecaptchaSecret)
    forM_ mCaptchaSecret $ \captchaSecret -> do
        let cr = CaptchaRequest captchaSecret (wfr ^. gRecaptchaResponse)
        logInfo "Found a secret for recaptcha in config, attempting validation"
        captchaResp <- liftIO $ captchaRequest cr
        logInfo ("Recaptcha result: " <> (captchaResp ^. to show . packed))
        unless (captchaResp ^. success) $ do
            throwError $ err500 { errBody = captchaResp ^. errorCodes . to show . packedChars }
    let wr = WithdrawlRequest (wfr ^. wfAddress)
    withdraw wr

-- | Handler for the withdrawl of ADA from the faucet
withdraw :: (MonadFaucet c m) => WithdrawlRequest -> m WithdrawlResult
withdraw wr = withSublogger (LoggerName "withdraw") $ do
    logInfo "Attempting to send ADA"
    resp <- Client.withdraw (wr ^. wAddress)
    case resp of
        Left err -> do
            logError ("Error withdrawing " <> (wr ^. to show . packed)
                                           <> " error: "
                                           <> (err ^. to show . packed))
            return $ WithdrawlError (show err ^. packed)
        Right withDrawResp -> do
            let txn = wrData withDrawResp
                amount = unV1 $ txAmount txn
            logInfo ((withDrawResp ^. to show . packed) <> " withdrew: "
                                              <> (amount ^. to show . packed))
            incWithDrawn amount
            return $ WithdrawlSuccess txn

-- | Function to _deposit funds back into the faucet /not implemented/
_deposit :: (MonadFaucet c m) => DepositRequest -> m DepositResult
_deposit dr = withSublogger (LoggerName "_deposit") $ do
    -- decrWithDrawn (dr ^. dAmount)
    logInfo ((dr ^. to show . packed) <> " deposited")
    return DepositResult

-- | Serve the api, faucet form end point and a Raw endpoint for the html form
--
-- TODO: Get the server path from the config
faucetServer :: ServerT FaucetAPI M
faucetServer = withdraw :<|> formWithdraw :<|> (retag $ serveDirectoryWebApp ".")
