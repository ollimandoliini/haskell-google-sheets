-- | This module introduces a hierachy of type classes for Haskell values into sheet values.
module Web.Google.Sheets.Spreadsheets.Values.ToSheet where

import Data.Text (Text, pack)
import Data.Vector (Vector, fromList)
import Web.Google.Sheets.Spreadsheets.Values.Types (WriteSheetValue (..))

-- | `ToSheet` is the highest level of the encoding type class hiearchy. It is used to convert a value of type
-- `a` to a two-dimensional (a vector of vectors) value which can then be written to a sheet.
class ToSheet a where
  toSheet :: a -> Vector (Vector WriteSheetValue)

instance (ToSheetDimension a) => ToSheet (Vector a) where
  toSheet = fmap toSheetDimension

instance (ToSheetDimension a) => ToSheet [a] where
  toSheet = fmap toSheetDimension . fromList

-- | `ToSheetDimension` is the second level of the encoding type class hiearchy. It is used to convert a value of type
-- `a` to a one-dimensional (a vector of vectors) value which can then be written to a sheet.
-- It is commonly used when writing rows or columns of some user-defined type to a sheet.
class ToSheetDimension a where
  toSheetDimension :: a -> Vector WriteSheetValue

instance (ToSheetValue a) => ToSheetDimension (Vector a) where
  toSheetDimension = fmap toSheetValue

instance (ToSheetValue a) => ToSheetDimension [a] where
  toSheetDimension = fmap toSheetValue . fromList

-- | `ToSheetValue` is the third and the lowest level of the encoding type class hiearchy. It is used to convert a value of type @\a@.
-- into a `WriteSheetValue`.
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
