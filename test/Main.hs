{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import System.Environment
import Test.Hspec
import Web.Google.Sheets
import Web.Google.Sheets.Spreadsheets.Values.SheetValue (WriteSheetValue (..))

main :: IO ()
main = do
  token <- encodeUtf8 . pack <$> getEnv "AUTH_TOKEN"
  spreadSheetId <- pack <$> getEnv "SPREADSHEET_ID"
  sheetName <- pack <$> getEnv "SHEET_NAME"
  hspec $ do
    describe "appendValues" $ do
      it "appends a value to the sheet" $ do
        let range = RangeWithSheetName (Just (FullRange 0 0 2 0)) sheetName
            request =
              appendValues
                token
                Nothing
                spreadSheetId
                range
                UserEntered
                [[SheetDouble 10, SheetBool True, SheetString "Moro"]]
        runReq defaultHttpConfig request `shouldReturn` ()
