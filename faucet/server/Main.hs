{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Lens
import           Network.Wai.Handler.Warp (run)
import           Servant
import System.Environment (getArgs)
import Data.ByteString.Lazy as BSL
import Data.Aeson (eitherDecode)
import           System.Wlog (LoggerName (..), launchFromFile)
import Control.Monad.Except
-- import           Cardano.Wallet.API.V1.Types (PaymentSource (..), WalletId(..), AccountIndex)
import           Cardano.Faucet
import System.Remote.Monitoring (forkServer, serverMetricStore)
import           System.Remote.Monitoring.Statsd (forkStatsd)

main :: IO ()
main = do
  ekg <- forkServer "localhost" 8001
  args <- getArgs
  config <- case args of
    [ "--config", cfgFile ] -> do
      ecfg <- eitherDecode <$> BSL.readFile cfgFile
      either (error . ("Error decoding: " ++)) return ecfg
    _ -> error "Need a --config argument pointing to a json file"
  fEnv <- runInitLogger config $ initEnv config (serverMetricStore ekg)
  _statsd <- forkStatsd (config ^. fcStatsdOpts . _Wrapped') (fEnv ^. feStore)
  run (config ^. fcPort) (serve serverAPI $ s fEnv)
  where
      nat :: FaucetEnv -> M a -> Handler a
      nat e = Handler . ExceptT . runM e
      s env = hoistServer serverAPI (nat env) server
      -- runInitLogger :: FaucetConfig -> LoggerNameBox IO FaucetEnv -> IO FaucetEnv
      runInitLogger c = launchFromFile (c ^. fcLoggerConfigFile) (LoggerName "faucet")
