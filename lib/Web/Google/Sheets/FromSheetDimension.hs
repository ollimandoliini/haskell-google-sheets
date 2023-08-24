module Web.Google.Sheets.FromSheetDimension where

import Data.Text (Text)
import Data.Vector (Vector, toList)
import Web.Google.Sheets.FromSheetValue (FromSheetValue (fromSheetValue))

class FromSheetDimension a where
  fromSheetDimension :: Vector Text -> Either String a

instance (FromSheetValue a) => FromSheetDimension (Vector a) where
  fromSheetDimension vec = traverse fromSheetValue vec

instance (FromSheetValue a) => FromSheetDimension [a] where
  fromSheetDimension vec = toList <$> traverse fromSheetValue vec
