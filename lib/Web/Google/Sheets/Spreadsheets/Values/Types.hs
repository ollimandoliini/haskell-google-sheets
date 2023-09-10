{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.Spreadsheets.Values.Types
  ( Dimension (..)
  , GetValueParams (..)
  , defaultGetValueParams
  , ValueRenderOption (..)
  , ValueInputOption (..)
  , Range (..)
  , SheetRange (..)
  , DatetimeRenderOption (..)
  , ReadValueRange (..)
  , WriteSheetValue (..)
  , ReadSheetValue (..)
  )
where

import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.!=), (.:?))
import Data.Aeson.Types (ToJSON (toJSON), Value (String))
import Data.Text (Text)
import Data.Vector (Vector, empty, singleton)
import Numeric.Natural (Natural)

-- | A range in a sheet where data is read from or written to.
-- https://developers.google.com/sheets/api/guides/concepts#expandable-1
data Range
  = RangeWithSheetName
      (Maybe SheetRange)
      -- ^ Leave empty to select the whole sheet.
      Text
  | RangeWithDefaultSheet SheetRange
  deriving (Show)

-- | Zero-based indices for rows and columns.
-- https://developers.google.com/sheets/api/guides/concepts#expandable-1
data SheetRange
  = -- | A range that specifies bounds of all dimensions.
    --
    --   @A1:C2@ = @FullRange 0 0 2 1@
    FullRange
      Natural
      -- ^ Start column
      Natural
      -- ^ Start row
      Natural
      -- ^ End column
      Natural
      -- ^ End row
  | -- | A range range that selects all values from a range of rows
    --
    --   @1:5@ = @RowRange 0 4@
    RowRange
      Natural
      -- ^ Start row
      Natural
      -- ^ End row
  | -- | A range range that selects all values from a range of columns
    --
    --   @B:D@ = @ColumnRange 1 3@
    ColumnRange
      Natural
      -- ^ Start column
      Natural
      -- ^ End column
  | -- | A range that selects all values from a range of columns with optional start and end rows.
    --
    --   @A2:D@ = @PartialColumnRange 0 (Just 1) 4 Nothing
    PartialColumnRange
      Natural
      -- ^ Start column
      (Maybe Natural)
      -- ^ Start row
      Natural
      -- ^ End column
      (Maybe Natural)
      -- ^ End row
  deriving (Show)

-- | A value that's read from a sheet. The Sheets API always returns a string.
newtype ReadSheetValue
  = ReadSheetValue Text
  deriving (Show)

instance FromJSON ReadSheetValue where
  parseJSON (String s) = pure $ ReadSheetValue s
  parseJSON val = fail $ "Could not parse ReadSheetValue from: " <> show val

-- | Raw values returned by the Sheets API.
newtype ReadValueRange = ReadValueRange {values :: Vector (Vector ReadSheetValue)}

instance FromJSON ReadValueRange where
  parseJSON =
    withObject "ReadValueRange" $
      \o -> ReadValueRange <$> o .:? "values" .!= singleton empty

-- | A value that's written to a sheet.
data WriteSheetValue
  = SheetDouble Double
  | SheetString Text
  | SheetBool Bool
  deriving (Show)

instance ToJSON WriteSheetValue where
  toJSON (SheetDouble d) = toJSON d
  toJSON (SheetString s) = toJSON s
  toJSON (SheetBool b) = toJSON b

-- | https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/get#query-parameters
data GetValueParams = GetValueParams
  { majorDimension :: Dimension
  , valueRenderOption :: ValueRenderOption
  , datetimeRenderOption :: DatetimeRenderOption
  }

-- | Default `GetValueParams` for `getValues` and `getValueRange` functions.
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
