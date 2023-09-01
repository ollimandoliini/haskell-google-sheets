{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.Spreadsheets.Values.FromSheet where

import Data.Text (Text, unpack)
import Data.Vector (Vector, toList)
import Text.Read (readEither)
import Web.Google.Sheets.Spreadsheets.Values.SheetValue (ReadSheetValue (..))

class FromSheet a where
  fromSheet :: Vector (Vector ReadSheetValue) -> Either String a

instance (FromSheetDimension a) => FromSheet (Vector a) where
  fromSheet vec = traverse fromSheetDimension vec

instance (FromSheetDimension a) => FromSheet [a] where
  fromSheet vec = traverse fromSheetDimension (toList vec)

class FromSheetDimension a where
  fromSheetDimension :: Vector ReadSheetValue -> Either String a

instance (FromSheetValue a) => FromSheetDimension (Vector a) where
  fromSheetDimension vec = traverse fromSheetValue vec

instance (FromSheetValue a) => FromSheetDimension [a] where
  fromSheetDimension vec = toList <$> traverse fromSheetValue vec

class FromSheetValue a where
  fromSheetValue :: ReadSheetValue -> Either String a

instance FromSheetValue ReadSheetValue where
  fromSheetValue = pure

instance FromSheetValue Bool where
  fromSheetValue (ReadSheetValue "TRUE") = Right True
  fromSheetValue (ReadSheetValue "FALSE") = Right False
  fromSheetValue val = Left $ "Could not convert '" <> show val <> "' to Bool"

instance FromSheetValue Text where
  fromSheetValue (ReadSheetValue s) = Right s

instance FromSheetValue String where
  fromSheetValue (ReadSheetValue s) = Right . unpack $ s

instance FromSheetValue Double where
  fromSheetValue (ReadSheetValue s) = readEither . unpack $ s
