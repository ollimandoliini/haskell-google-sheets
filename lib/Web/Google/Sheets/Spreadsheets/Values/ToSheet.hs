module Web.Google.Sheets.Spreadsheets.Values.ToSheet where

import Data.Text (Text, pack)
import Data.Vector (Vector, fromList)
import Web.Google.Sheets.Spreadsheets.Values.SheetValue (WriteSheetValue (..))

class ToSheet a where
  toSheet :: a -> Vector (Vector WriteSheetValue)

instance (ToSheetDimension a) => ToSheet (Vector a) where
  toSheet = fmap toSheetDimension

instance (ToSheetDimension a) => ToSheet [a] where
  toSheet = fmap toSheetDimension . fromList

class ToSheetDimension a where
  toSheetDimension :: a -> Vector WriteSheetValue

instance (ToSheetValue a) => ToSheetDimension (Vector a) where
  toSheetDimension = fmap toSheetValue

instance (ToSheetValue a) => ToSheetDimension [a] where
  toSheetDimension = fmap toSheetValue . fromList

class ToSheetValue a where
  toSheetValue :: a -> WriteSheetValue

instance ToSheetValue Double where
  toSheetValue = SheetDouble

instance ToSheetValue Float where
  toSheetValue = SheetDouble . realToFrac

instance ToSheetValue Integer where
  toSheetValue = SheetDouble . fromInteger

instance ToSheetValue Int where
  toSheetValue = SheetDouble . fromIntegral

instance ToSheetValue Text where
  toSheetValue = SheetString

instance ToSheetValue String where
  toSheetValue = SheetString . pack

instance ToSheetValue Bool where
  toSheetValue = SheetBool

instance ToSheetValue WriteSheetValue where
  toSheetValue = id
