{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.Types
  ( Cell (..)
  , Dimension (..)
  , ValueRangeParams (..)
  , defaultValueRangeParams
  , ValueRange (..)
  , ValueRenderOption (..)
  , ValueInputOption (..)
  , Range (..)
  , DatetimeRenderOption (..)
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=), withArray)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | Zero-based indices of a single cell
data Cell = Cell
  { row :: Natural
  , column :: Natural
  }
  deriving (Show)

-- | https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/get#query-parameters
data ValueRangeParams = ValueRangeParams
  { majorDimension :: Maybe Dimension
  , valueRenderOption :: Maybe ValueRenderOption
  , datetimeRenderOption :: Maybe DatetimeRenderOption
  }

defaultValueRangeParams :: ValueRangeParams
defaultValueRangeParams = ValueRangeParams Nothing Nothing Nothing

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
  = -- | Range in the default sheet
    RangeDefaultSheet Text
  | -- | Range in the specified sheet
    RangeWithSheet {range :: Text, sheet :: Text}
  | -- | Full sheet
    FullSheet Text

-- | Main type for reading and writing values to a spreadsheet
newtype ValueRange = ValueRange
  { values :: Vector (Vector Text)
  }
  deriving (Show, Generic)

instance FromJSON ValueRange where
  parseJSON = withObject "ValueRange" $ \o -> ValueRange <$> (o .: "values")

instance ToJSON ValueRange where
  toJSON (ValueRange values) = object ["values" .= values]


