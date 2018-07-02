{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wall #-}

module Cardano.Faucet.Types.Recaptcha
    ( CaptchaSecret(..)
    , CaptchaRequest(..), secret, response
    , CaptchaResponse(..), success, challengeTS, hostname, errorCodes
    , WithdrawlFormRequest(..), wfAddress, gRecaptchaResponse
    , captchaRequest) where

import           Control.Lens hiding ((.=))
import           Data.Maybe
import           Data.String (IsString)
import           Data.Swagger
import           Data.Typeable
import           Network.Wreq (FormParam (..))
import qualified Network.Wreq as Wreq
-- import Data.Proxy
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)
import           Web.FormUrlEncoded

import           Cardano.Wallet.API.V1.Types (V1 (..))
import           Pos.Core (Address (..))

--------------------------------------------------------------------------------
newtype CaptchaSecret = CaptchaSecret Text deriving (Show, IsString)

makeWrapped ''CaptchaSecret

newtype GCaptchaResponse = GCaptchaResponse Text deriving (Show, IsString)

makeWrapped ''GCaptchaResponse
--------------------------------------------------------------------------------
-- | Request for sending to google to validate recaptcha
data CaptchaRequest = CaptchaRequest {
    -- | The secret given by google
    _secret   :: CaptchaSecret
    -- | The "g-recaptcha-response" field sent by the form
  , _response :: GCaptchaResponse
  } deriving (Generic)

makeLenses ''CaptchaRequest

--------------------------------------------------------------------------------
-- | A request to withdraw ADA from the faucet wallet
data WithdrawlFormRequest = WithdrawlFormRequest {
    -- | The address to send the ADA to
    _wfAddress          :: !(V1 Address)
    -- | The "g-recaptcha-response" field sent by the form
  , _gRecaptchaResponse :: !GCaptchaResponse
  } deriving (Show, Typeable, Generic)

makeLenses ''WithdrawlFormRequest

instance FromJSON WithdrawlFormRequest where
  parseJSON = withObject "WithdrawlFormRequest" $ \v -> WithdrawlFormRequest
    <$> v .: "address"
    <*> (GCaptchaResponse <$> v .: "g-recaptcha-response")

instance FromForm WithdrawlFormRequest where
    fromForm f = WithdrawlFormRequest
      <$> parseUnique "address" f
      <*> (GCaptchaResponse <$> parseUnique "g-recaptcha-response" f)

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
-- | Response from google to being sent a 'CaptchaRequest'
data CaptchaResponse = CaptchaResponse {
    -- | Was the recatcha validated as not coming from a bot
    _success     :: Bool
    -- | The time of the challenge
    --
    -- (Maybe because this isn't present if there are errors)
  , _challengeTS :: Maybe UTCTime
    -- | The hostname serving the form
    --
    -- (Maybe because this isn't present if there are errors)
  , _hostname    :: Maybe Text
    -- | Any errors present
  , _errorCodes  :: [Text]
  } deriving (Show)

makeLenses ''CaptchaResponse

instance FromJSON CaptchaResponse where
    parseJSON = withObject "CaptchaResponse" $ \v -> CaptchaResponse
      <$> v .: "success"
      <*> v .:? "challenge_ts"
      <*> v .:? "hostname"
      <*> (fromMaybe [] <$> v .:? "error-codes")


-- | Makes the 'CaptchaRequest' to google
captchaRequest :: CaptchaRequest -> IO CaptchaResponse
captchaRequest cr = do
    resp <- Wreq.asJSON =<< (Wreq.post "https://www.google.com/recaptcha/api/siteverify"
                             [ "secret" := cr ^. secret . _Wrapped
                             , "response" := cr ^. response . _Wrapped])
    return $ resp ^. Wreq.responseBody
