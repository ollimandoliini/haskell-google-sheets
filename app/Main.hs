{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Credentials
  ( createSignedJWT
  , getAccessToken
  , getPrivateKey
  )
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.Text (Text)
import Data.Vector (Vector, uncons)
import Network.HTTP.Req (MonadHttp (handleHttpException))
import Types (AccessToken (unAccessToken), AppError (AppHttpError), Email (Email), Scope (..))
import Web.Google.Sheets (Range (RangeDefaultSheet), getValue)
import Web.Google.Sheets.Types (defaultValueRangeParams)
import Web.Google.Sheets.FromSheetDimension (FromSheetDimension (fromSheetDimension))
import Web.Google.Sheets.FromSheetValue (FromSheetValue(fromSheetValue))

newtype App a = App
  { unApp :: ExceptT AppError IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError AppError
    )

instance MonadRandom App where
  getRandomBytes = liftIO . getRandomBytes

instance MonadHttp App where
  handleHttpException = throwError . AppHttpError

runApp :: App a -> IO ()
runApp =
  (runExceptT . unApp) >=> either throwIO (const $ pure ())

data MyRow = MyRow Text (Vector Bool) deriving (Show)

instance FromSheetDimension MyRow where
  fromSheetDimension vec =
    case uncons vec of
      Just (date, tail') -> do
        bools <- traverse fromSheetValue tail'
        pure (MyRow date bools)
      Nothing -> Left "Empty vector"

main :: IO ()
main = do
  runApp $ do
    privateKey <- getPrivateKey "creds.json"
    signedJWT <-
      createSignedJWT
        (Email "padel-sheet@things-394312.iam.gserviceaccount.com")
        Nothing
        [Scope "https://www.googleapis.com/auth/spreadsheets"]
        privateKey
    accessToken <- unAccessToken <$> getAccessToken signedJWT
    value :: Either String [[Text]] <-
      getValue
        accessToken
        "1IgNaodgvvCcSvLXb9ZG7WdLX3L87hwmzWbIBIOQ9SMw"
        (RangeDefaultSheet "B2:E14")
        defaultValueRangeParams
    liftIO $ print value
