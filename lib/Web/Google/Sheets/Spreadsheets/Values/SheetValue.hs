module Web.Google.Sheets.Spreadsheets.Values.SheetValue where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Text (Text)

newtype ReadSheetValue
  = ReadSheetValue Text
  deriving (Show)

data WriteSheetValue
  = SheetDouble Double
  | SheetString Text
  | SheetBool Bool
  deriving (Show)

instance FromJSON ReadSheetValue where
  parseJSON (String s) = pure $ ReadSheetValue s
  parseJSON val = fail $ "Could not parse ReadSheetValue from: " <> show val

instance ToJSON WriteSheetValue where
  toJSON (SheetDouble d) = toJSON d
  toJSON (SheetString s) = toJSON s
  toJSON (SheetBool b) = toJSON b
