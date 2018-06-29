{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Cardano.Faucet.Swagger
    ( FaucetDoc
    , swaggerServer
    , faucetDocAPI
    , faucetHandler
    ) where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Proxy
import           Data.String (fromString)
import           Data.Swagger
import qualified Data.Text as T
import           NeatInterpolation
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI (SwaggerSchemaUI)

import           Cardano.Wallet.API.V1.Swagger
import           Pos.Core.Update (SoftwareVersion)
import           Pos.Update.Configuration (HasUpdateConfiguration,
                                           curSoftwareVersion)
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo,
                                       compileInfo)

import           Cardano.Faucet
import           Cardano.Faucet.Types
import           Servant

type FaucetDoc = SwaggerSchemaUI "docs" "swagger.json"

type FaucetDocAPI = FaucetDoc :<|> FaucetAPI


faucetDocAPI :: Proxy FaucetDocAPI
faucetDocAPI = Proxy

cardanoVersion :: T.Text
cardanoVersion = "cardano-sl:0"

faucetMD :: CompileTimeInfo -> T.Text
faucetMD CompileTimeInfo{..} = [text|
This is the faucet api documentation

The faucet is a component of the test net that allows users to request ADA from
a wallet to their own address for testing.

Software Version   | Git Revision
-------------------|-------------------
$cardanoVersion           | $ctiGitRevision

 |]

mkSwagger :: HasSwagger a
    => CompileTimeInfo
    -> Proxy a
    -> Swagger
mkSwagger compileInfo walletAPI = toSwagger walletAPI
  & info.title   .~ "Cardano Faucet API"
  & info.version .~ cardanoVersion
  & host ?~ "127.0.0.1:8090"
  & info.description ?~ (faucetMD compileInfo)
  & info.license ?~ ("MIT" & url ?~ URL "https://raw.githubusercontent.com/input-output-hk/cardano-sl/develop/lib/LICENSE")

swaggerServer :: (HasCompileInfo) => Server FaucetDoc
swaggerServer = swaggerSchemaUIServer (mkSwagger compileInfo faucetServerAPI)

faucetHandler :: HasCompileInfo => FaucetEnv -> Server FaucetDocAPI
faucetHandler env = swaggerServer
               :<|> hoistServer faucetServerAPI (nat env) faucetServer
  where
      nat :: FaucetEnv -> M a -> Handler a
      nat e = Handler . ExceptT . runM e
