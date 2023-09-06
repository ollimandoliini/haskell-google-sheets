{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (StrictByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getEnv)
import Test.Hspec
import Web.Google.Sheets
import Web.Google.Sheets.Spreadsheets.Values.SheetValue (WriteSheetValue (..))

getToken :: IO StrictByteString
getToken = encodeUtf8 . pack <$> getEnv "AUTH_TOKEN"

main = do
  hspec $ before getToken $ do
    describe "appendValues" $ do
      it "appends a value to the sheet" $ \token -> do
        let range = RangeWithSheetName (Just (FullRange 0 0 2 0)) "ghc-9.2.8"
            request =
              updateValues
                token
                Nothing
                "1PHUwechIwIa2lyw522dUbsvdVgh-6-eJSz_jVXuAKgw"
                range
                UserEntered
                [[SheetDouble 10, SheetBool True, SheetString "Moro"]]
        runReq defaultHttpConfig request `shouldReturn` ()

-- main :: IO ()
-- main = do
--   env <- getEnv "AUTH_TOKEN"
--   print env
--   error "sorry"
