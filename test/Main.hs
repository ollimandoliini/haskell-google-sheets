{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (empty, singleton)
import System.Environment
import Test.Hspec
import Web.Google.Sheets

main :: IO ()
main = do
  token <- encodeUtf8 . pack <$> getEnv "AUTH_TOKEN"
  spreadSheetId <- pack <$> getEnv "SPREADSHEET_ID"
  sheetName <- pack <$> getEnv "SHEET_NAME"
  hspec $ do
    describe "clearValues" $ do
      it "clears values from a previously populated range" $ do
        flip shouldReturn (ReadValueRange (singleton empty)) $ runReq defaultHttpConfig $ do
          let range = RangeWithSheetName (Just (ColumnRange 0 2)) sheetName
          updateValues token Nothing spreadSheetId range UserEntered [[SheetDouble 10, SheetBool True, SheetString "Moro"]]
          clearValues token Nothing spreadSheetId range
          getValueRange token Nothing spreadSheetId range defaultGetValueParams
