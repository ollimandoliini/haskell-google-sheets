{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON (..), Value, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.ByteString (StrictByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Crypto.JWT (AsError(_Error), Error)
import Control.Lens (Prism', prism')
import Network.HTTP.Req (HttpException)
import Control.Exception (Exception)


data AppError
    = AppServiceAccountDecodeError
    | AppPrivateKeyReadError
    | AppJWTError Error
    | AppHttpError HttpException
    deriving Show

instance Exception AppError

instance AsError AppError where
    _Error :: Prism' AppError Error
    _Error = prism' AppJWTError helper
      where
        helper :: AppError -> Maybe Error
        helper (AppJWTError err) = Just err
        helper _ = Nothing



newtype AccessToken = AccessToken
    { unAccessToken :: StrictByteString
    }
    deriving (Show)

instance FromJSON AccessToken where
    parseJSON :: Value -> Parser AccessToken
    parseJSON = withObject "AccessToken" $ \o -> AccessToken . encodeUtf8 <$> o .: "access_token"

newtype Email = Email
    { unEmail :: Text
    }

newtype Scope = Scope
    { unScope :: Text
    }
