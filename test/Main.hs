{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM_)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (empty, fromList, replicate, singleton)
import System.Environment
import Test.Hspec
import Web.Google.Sheets
import Web.Google.Sheets.Spreadsheets.Values (InsertDataOption (InsertRows), ValueRenderOption (UnformattedValue))
import Prelude hiding (replicate)

main :: IO ()
main = do
  token <- encodeUtf8 . pack <$> getEnv "AUTH_TOKEN"
  quotaProject <- (fmap . fmap) pack (lookupEnv "QUOTA_PROJECT")
  spreadSheetId <- pack <$> getEnv "SPREADSHEET_ID"
  sheetName <- pack <$> getEnv "SHEET_NAME"
  let setup = runReq defaultHttpConfig (clearValues token quotaProject spreadSheetId (RangeWithSheetName Nothing sheetName))

  hspec $ do
    describe "clearValues" $ before setup $ do
      it "clears values from a previously populated range" $ do
        flip shouldReturn (ReadValueRange (singleton empty)) $ runReq defaultHttpConfig $ do
          let range = RangeWithSheetName (Just (ColumnRange 0 2)) sheetName
          updateValues token quotaProject spreadSheetId range UserEntered [[SheetDouble 10, SheetBool True, SheetString "something"]]
          clearValues token quotaProject spreadSheetId range
          getValueRange token quotaProject spreadSheetId range defaultGetValueParams

    describe "updateValues" $ before setup $ do
      it "updates values" $ do
        let range = RangeWithSheetName (Just (FullRange 0 0 3 0)) sheetName
            op = runReq defaultHttpConfig $ do
              updateValues token quotaProject spreadSheetId range UserEntered [[SheetDouble 10, SheetBool True, SheetString "something"]]
              updateValues token quotaProject spreadSheetId range UserEntered [[SheetDouble 10, SheetBool True, SheetString "something else"]]
              getValueRange token quotaProject spreadSheetId range (defaultGetValueParams {valueRenderOption = UnformattedValue})
            expection = ReadValueRange (singleton (fromList [SheetDouble 10, SheetBool True, SheetString "something else"]))
        op `shouldReturn` expection

    describe "appendValues" $ before setup $ do
      it "appends values to sheet" $ do
        let range = RangeWithSheetName (Just (ColumnRange 0 2)) sheetName
            op = runReq defaultHttpConfig $ do
              appendValues token quotaProject spreadSheetId range UserEntered InsertRows [[SheetDouble 10, SheetBool True, SheetString "something"]]
              getValueRange token quotaProject spreadSheetId range (defaultGetValueParams {valueRenderOption = UnformattedValue})
        op `shouldReturn` ReadValueRange (singleton (fromList [SheetDouble 10, SheetBool True, SheetString "something"]))

      it "appends values to to sheet multiple times" $ do
        let range = RangeWithSheetName (Just (ColumnRange 0 2)) sheetName
            op = runReq defaultHttpConfig $ do
              replicateM_ 3 (appendValues token quotaProject spreadSheetId range UserEntered InsertRows [[SheetDouble 10, SheetBool True, SheetString "something"]])
              getValueRange token quotaProject spreadSheetId range (defaultGetValueParams {valueRenderOption = UnformattedValue})
            expection = ReadValueRange $ replicate 3 (fromList [SheetDouble 10, SheetBool True, SheetString "something"])
        op `shouldReturn` expection

    describe "FromSheet/ToSheet" $ before setup $ do
      it "encodes and decodes Int properly" $ do
        let range = RangeWithSheetName (Just (FullRange 0 0 3 0)) sheetName
            op = runReq defaultHttpConfig $ do
              updateValues token quotaProject spreadSheetId range UserEntered ([[1, 2, 3]] :: [[Int]])
              getValues token quotaProject spreadSheetId range (defaultGetValueParams {valueRenderOption = UnformattedValue})
            expection = Right [[1 :: Int, 2, 3]]
        op `shouldReturn` expection

      it "encodes and decodes Double properly" $ do
        let range = RangeWithSheetName (Just (FullRange 0 0 3 0)) sheetName
            op = runReq defaultHttpConfig $ do
              updateValues token quotaProject spreadSheetId range UserEntered ([[1.1, 2.2, 3.3]] :: [[Double]])
              getValues token quotaProject spreadSheetId range (defaultGetValueParams {valueRenderOption = UnformattedValue})
            expection = Right [[1.1 :: Double, 2.2, 3.3]]
        op `shouldReturn` expection
