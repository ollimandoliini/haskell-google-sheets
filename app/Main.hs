{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Vector (Vector)
import Network.HTTP.Req (defaultHttpConfig, runReq)
import Web.Google.Sheets.Spreadsheets.Values.SheetValue
  ( ReadSheetValue
  , WriteSheetValue (..)
  )
import Web.Google.Sheets.Spreadsheets.Values (getValues)
import Web.Google.Sheets.Types
  ( Range (RangeWithSheetName)
  , SheetRange (..)
  , ValueInputOption (..)
  , defaultGetValueParams
  )
import Web.Google.Sheets (updateValues)

data MyRow = MyRow Text (Vector Bool) deriving (Show)

-- instance FromSheetDimension MyRow where
--   fromSheetDimension vec =
--     case uncons vec of
--       Just (date, tail') -> do
--         bools <- traverse fromSheetValue tail'
--         pure (MyRow date bools)
--       Nothing -> Left "Empty vector"

main :: IO ()
main = do
  let accessToken = "ya29.a0AfB_byCOuZo-P4YuNzLB-2uW_5vhhRvPK-eyktMy83e-81CAssvlKE0sOUtGbucMde-HHR-qU-Qu7aidmwPDxa7CixsVxVDivKUwbwRvt24Pwu8MTQPJippjmKx9C3qmCBlDqaCUEmZvL1qVxZHc3AcpshBg1w9Fom7VOH9-OAaCgYKAYMSARASFQHsvYlsDzp8OI6nxnvZml1CHUWIIw0177"
      quotaProject = Just "things-394312"
      spreadsheetId = "1IgNaodgvvCcSvLXb9ZG7WdLX3L87hwmzWbIBIOQ9SMw"
      range = RangeWithSheetName (Just (FullRange 5 0 6 2)) "Taulukko1"
  runReq defaultHttpConfig
    $ updateValues
      accessToken
      quotaProject
      spreadsheetId
      range
      UserEntered
      [[SheetDouble 10, SheetBool True, SheetString "Moro"]]

  value :: Either String [[ReadSheetValue]] <-
    runReq defaultHttpConfig
      $ getValues
        accessToken
        quotaProject
        spreadsheetId
        range
        defaultGetValueParams
  print value
