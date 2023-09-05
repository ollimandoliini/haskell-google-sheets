{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (StrictByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getEnv)
import Test.Hspec
import Web.Google.Sheets

getToken :: IO StrictByteString
getToken = encodeUtf8 . pack <$> getEnv "AUTH_TOKEN"

main = do
  hspec $ before getToken $ do
    describe "appendValues" $ do
      it "appends a value to the sheet" $ \token -> do
        let range = RangeWithSheetName (Just (ColumnRange 1 1)) "ghc-9.2.8"
        runReq
          defaultHttpConfig
          (appendValues token "1PHUwechIwIa2lyw522dUbsvdVgh-6-eJSz_jVXuAKgw" range Raw [["Moro" :: Text, "Moro"]])
          `shouldReturn` ()

-- main :: IO ()
-- main = do
--   env <- getEnv "AUTH_TOKEN"
--   print env
--   error "sorry"
