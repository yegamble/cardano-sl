{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

module Cardano.Faucet.Types.Recaptcha
    ( CaptchaRequest(..), secret, response
    , CaptchaResponse(..), success, challengeTS, hostname, errorCodes
    , WithdrawlFormRequest(..), wfAddress, gRecaptchaResponse
    , captchaRequest) where

import qualified Network.Wreq as Wreq
import Network.Wreq (FormParam(..))
import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Typeable
import           Data.Swagger
-- import Data.Proxy
import GHC.Generics (Generic)
import Data.Text (Text)
import Web.FormUrlEncoded
import Data.Aeson
import Data.Time.Clock (UTCTime)

import           Cardano.Wallet.API.V1.Types (V1 (..))
import           Pos.Core (Address (..))
--------------------------------------------------------------------------------
data CaptchaRequest = CaptchaRequest {
  _secret :: Text
, _response :: Text
} deriving (Generic)

makeLenses ''CaptchaRequest

instance ToJSON CaptchaRequest

--------------------------------------------------------------------------------
-- | A request to withdraw ADA from the faucet wallet
data WithdrawlFormRequest = WithdrawlFormRequest {
    _wfAddress :: !(V1 Address)
  , _gRecaptchaResponse :: !Text
  } deriving (Show, Typeable, Generic)

makeLenses ''WithdrawlFormRequest

instance FromJSON WithdrawlFormRequest where
  parseJSON = withObject "WithdrawlFormRequest" $ \v -> WithdrawlFormRequest
    <$> v .: "address"
    <*> v .: "g-recaptcha-response"

instance FromForm WithdrawlFormRequest where
    fromForm f = WithdrawlFormRequest
      <$> parseUnique "address" f
      <*> parseUnique "g-recaptcha-response" f

instance ToSchema WithdrawlFormRequest where
    declareNamedSchema _ = do
        addrSchema <- declareSchemaRef (Proxy :: Proxy (V1 Address))
        recaptchaSchema <- declareSchemaRef (Proxy :: Proxy Text)
        return $ NamedSchema (Just "WithdrawlFormRequest") $ mempty
          & type_ .~ SwaggerObject
          & properties .~ (mempty & at "address" ?~ addrSchema
                                  & at "g-recaptcha-response" ?~ recaptchaSchema)
          & required .~ ["address", "g-recaptcha-response"]

--------------------------------------------------------------------------------
data CaptchaResponse = CaptchaResponse {
    _success :: Bool
  , _challengeTS :: Maybe UTCTime
  , _hostname :: Maybe Text
  , _errorCodes :: [Text]
  } deriving (Show)

makeLenses ''CaptchaResponse

instance FromJSON CaptchaResponse where
    parseJSON = withObject "CaptchaResponse" $ \v -> CaptchaResponse
      <$> v .: "success"
      <*> v .:? "challenge_ts"
      <*> v .:? "hostname"
      <*> (fromMaybe [] <$> v .:? "error-codes")


captchaRequest :: CaptchaRequest -> IO CaptchaResponse
captchaRequest cr = do
    resp <- Wreq.asJSON =<< (Wreq.post "https://www.google.com/recaptcha/api/siteverify"
                             [ "secret" := cr ^. secret
                             , "response" := cr ^. response])
    return $ resp ^. Wreq.responseBody
