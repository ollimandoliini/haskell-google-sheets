{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.FromSheetValue where

import Data.Text (Text, unpack)

class FromSheetValue a where
  fromSheetValue :: Text -> Either String a

instance FromSheetValue Bool where
  fromSheetValue "TRUE" = Right True
  fromSheetValue "FALSE" = Right False
  fromSheetValue s = Left $ "Unexpected value: '" <> unpack s <> "'. Expected TRUE or FALSE."

instance FromSheetValue Text where
  fromSheetValue = Right

instance FromSheetValue String where
  fromSheetValue = Right . unpack
