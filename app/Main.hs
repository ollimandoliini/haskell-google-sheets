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
import Network.HTTP.Req (MonadHttp (handleHttpException))
import Types (AccessToken (unAccessToken), AppError (AppHttpError), Email (Email), Scope (..))
import Web.Google.Sheets (getCellValue)
import Web.Google.Sheets.Types (Cell (Cell), defaultValueRangeParams)

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

main :: IO ()
main = do
  runApp $ do
    privateKey <- getPrivateKey "creds.json"
    signedJWT <- createSignedJWT (Email "padel-sheet@things-394312.iam.gserviceaccount.com") Nothing [Scope "https://www.googleapis.com/auth/spreadsheets"] privateKey
    accessToken <- unAccessToken <$> getAccessToken signedJWT
    value <-
      getCellValue
        accessToken
        "1IgNaodgvvCcSvLXb9ZG7WdLX3L87hwmzWbIBIOQ9SMw"
        (Cell 0 0)
        Nothing
        -- (SheetAndRange "Taulukko1" "A1:A2")
        defaultValueRangeParams
    liftIO $ print value
