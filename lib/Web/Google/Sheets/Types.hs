{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.Types
  ( Dimension (..)
  , GetValueParams (..)
  , defaultGetValueParams
  , ValueRenderOption (..)
  , ValueInputOption (..)
  , Range (..)
  , SheetRange (..)
  , DatetimeRenderOption (..)
  , rangeToText
  , ReadValueRange (..)
  )
where

import Data.Aeson (FromJSON, parseJSON, withObject, (.!=), (.:?))
import Data.Text (Text, pack)
import Data.Vector (Vector, empty, singleton)
import Numeric.Natural (Natural)
import Web.Google.Sheets.Spreadsheets.Values.SheetValue (ReadSheetValue)

-- | https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/get#query-parameters
data GetValueParams = GetValueParams
  { majorDimension :: Dimension
  , valueRenderOption :: ValueRenderOption
  , datetimeRenderOption :: DatetimeRenderOption
  }

defaultGetValueParams :: GetValueParams
defaultGetValueParams = GetValueParams Row FormattedValue SerialNumber

-- | https://developers.google.com/sheets/api/reference/rest/v4/Dimension
data Dimension
  = Row
  | Column
  deriving (Show)

-- | https://developers.google.com/sheets/api/reference/rest/v4/ValueRenderOption
data ValueRenderOption
  = FormattedValue
  | UnformattedValue
  | Formula

-- | https://developers.google.com/sheets/api/reference/rest/v4/DateTimeRenderOption
data DatetimeRenderOption
  = SerialNumber
  | FormattedString

-- | https://developers.google.com/sheets/api/reference/rest/v4/ValueInputOption
data ValueInputOption
  = Raw
  | UserEntered

data Range
  = RangeWithSheetName (Maybe SheetRange) Text
  | RangeWithDefaultSheet SheetRange
  deriving (Show)

data SheetRange
  = FullRange Natural Natural Natural Natural
  | RowRange Natural Natural
  | ColumnRange Natural Natural
  | PartialColumnRange Natural (Maybe Natural) Natural (Maybe Natural)
  deriving (Show)

coordinatesToRange :: Natural -> Natural -> Text
coordinatesToRange column row =
  enumerateColumns
    !! fromIntegral column
    <> (pack . show $ (row + 1))

columnToText :: Natural -> Text
columnToText column = enumerateColumns !! fromIntegral column

rowToText :: Natural -> Text
rowToText = pack . show . (+ 1)

-- | *NOTE* Produces an infinite list
enumerateColumns :: [Text]
enumerateColumns = pack <$> go letters
  where
    letters = (: []) <$> ['A' .. 'Z']
    go xs = xs ++ go [a : b | a <- ['A' .. 'Z'], b <- xs]

rangeToText :: Range -> Text
rangeToText (RangeWithDefaultSheet sheetRange) = sheetRangeToText sheetRange
rangeToText (RangeWithSheetName (Just sheetRange) sheetName) = "'" <> sheetName <> "'!" <> sheetRangeToText sheetRange
rangeToText (RangeWithSheetName Nothing sheetName) = sheetName

sheetRangeToText :: SheetRange -> Text
sheetRangeToText (FullRange startColumn startRow endColumn endRow) =
  coordinatesToRange startColumn startRow <> ":" <> coordinatesToRange endColumn endRow
sheetRangeToText (RowRange startRow endRow) = rowToText startRow <> ":" <> rowToText endRow
sheetRangeToText (ColumnRange startColumn endColumn) = columnToText startColumn <> ":" <> columnToText endColumn
sheetRangeToText (PartialColumnRange startColumn mStartRow endColumn mEndRow) =
  columnToText startColumn <> maybe "" rowToText mStartRow <> ":" <> columnToText endColumn <> maybe "" rowToText mEndRow

newtype ReadValueRange = ReadValueRange {values :: Vector (Vector ReadSheetValue)}

instance FromJSON ReadValueRange where
  parseJSON =
    withObject "ReadValueRange"
      $ \o -> ReadValueRange <$> o .:? "values" .!= singleton empty
