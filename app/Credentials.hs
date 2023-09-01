{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Credentials (createSignedJWT, getAccessToken, getPrivateKey) where

import Control.Lens ((#), (&), (?~))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Time (MonadTime (..))
import Crypto.JOSE (Alg (RS256), MonadRandom)
import Crypto.JWT (Audience (Audience), HasClaimsSet (..), NumericDate (NumericDate), SignedJWT, addClaim, emptyClaimsSet, encodeCompact, fromRSA, newJWSHeader, signClaims, string)
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (Value, eitherDecodeFileStrict, withObject, (.:))
import Data.Aeson qualified as Aeson (Value (String))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 qualified as B
import Data.Text (Text, unwords)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (addUTCTime)
import Data.X509 qualified as X509
import Data.X509.Memory (readKeyFileFromMemory)
import Network.HTTP.Req (POST (POST), ReqBodyUrlEnc (ReqBodyUrlEnc), defaultHttpConfig, https, jsonResponse, req, responseBody, runReq, (/:), (=:))
import Types (AccessToken (unAccessToken), AppError (AppPrivateKeyReadError, AppServiceAccountDecodeError), Email (Email), Scope (unScope))
import Prelude hiding (unwords)

getPrivateKey :: (MonadIO m, MonadError AppError m) => FilePath -> m PrivateKey
getPrivateKey file = do
  eVal <- liftIO $ eitherDecodeFileStrict file
  case eVal of
    Left _ -> throwError AppServiceAccountDecodeError
    Right v -> case parseEither extractPrivateKey v of
      Left _ -> throwError AppPrivateKeyReadError
      Right privateKeyString ->
        case parsePrivateKey $ B.pack privateKeyString of
          Just pk -> pure pk
          Nothing -> throwError AppPrivateKeyReadError
  where
    extractPrivateKey :: Value -> Parser String
    extractPrivateKey =
      withObject "ServiceAccount" $ \o -> o .: "private_key"

    parsePrivateKey :: StrictByteString -> Maybe PrivateKey
    parsePrivateKey bs =
      case readKeyFileFromMemory bs of
        [X509.PrivKeyRSA key] -> Just key
        _ -> Nothing

createSignedJWT
  :: ( MonadIO m
     , MonadRandom m
     , MonadError AppError m
     )
  => Email
  -> Maybe Email
  -> [Scope]
  -> PrivateKey
  -> m SignedJWT
createSignedJWT (Email issuer) msub scopes key = do
  t <- liftIO currentTime
  signClaims jwk header (claims t)
  where
    jwk = fromRSA key
    header = newJWSHeader ((), RS256)
    claims t =
      emptyClaimsSet
        & claimIss ?~ (string # issuer)
        & maybe id (\(Email sub) -> claimSub ?~ (string # sub)) msub
        & claimAud ?~ Audience ["https://www.googleapis.com/oauth2/v4/token"]
        & claimIat ?~ NumericDate t
        & claimExp ?~ NumericDate (addUTCTime 3600 t)
        & addClaim "scope" (Aeson.String $ unwords (unScope <$> scopes))

getAccessToken :: (MonadIO m) => SignedJWT -> m StrictByteString
getAccessToken signedJwt = do
  let tokenUrl = https "oauth2.googleapis.com" /: "token"
      tokenPayload =
        ReqBodyUrlEnc
          $ ("grant_type" =: ("urn:ietf:params:oauth:grant-type:jwt-bearer" :: Text))
          <> ("assertion" =: decodeUtf8 (encodeCompact signedJwt))
  unAccessToken . responseBody <$> runReq defaultHttpConfig (req POST tokenUrl tokenPayload jsonResponse mempty)
