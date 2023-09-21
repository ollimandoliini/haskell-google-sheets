{-# LANGUAGE OverloadedStrings #-}

-- | This module introduces a hierachy of type classes for converting sheet values into Haskell values.
module Web.Google.Sheets.Spreadsheets.Values.FromSheet where

import Data.Text (Text, unpack, pack)
import Data.Vector (Vector, toList)
import Text.Read (readEither)
import Web.Google.Sheets.Spreadsheets.Values.Types (SheetValue (..))
import Data.Text.Read (decimal)

-- | `FromSheet` is the highest level of the decoding type class hiearchy. It is used to convert a two-dimensional
-- (a vector of vectors) value into some value `a`.
class FromSheet a where
  fromSheet :: Vector (Vector SheetValue) -> Either String a

instance (FromSheetDimension a) => FromSheet (Vector a) where
  fromSheet vec = traverse fromSheetDimension vec

instance (FromSheetDimension a) => FromSheet [a] where
  fromSheet vec = traverse fromSheetDimension (toList vec)

-- | `FromSheetDimension` is the second level of the decoding type class hiearchy. It is used to convert a one-dimensional
-- (a vectors) value into some value @a@. Most commonly this type class is used when you sheet contains uniform records on
-- rows or columns. In such case you might want to define an instance of @FromSheetDimension MyType@ and then you can
-- use `getValues` to retrieve @Vector MyType@ from a sheet.
class FromSheetDimension a where
  fromSheetDimension :: Vector SheetValue -> Either String a

instance (FromSheetValue a) => FromSheetDimension (Vector a) where
  fromSheetDimension vec = traverse fromSheetValue vec

instance (FromSheetValue a) => FromSheetDimension [a] where
  fromSheetDimension vec = toList <$> traverse fromSheetValue vec

-- | `FromSheetValue` is the third and the lowest level of the decoding type class hiearchy. It is used to convert a single
-- value into some value @a@.
class FromSheetValue a where
  fromSheetValue :: SheetValue -> Either String a

instance FromSheetValue SheetValue where
  fromSheetValue = pure

instance FromSheetValue Bool where
  fromSheetValue (SheetBool True) = Right True
  fromSheetValue (SheetBool False) = Right False
  fromSheetValue val = Left $ "Could not convert '" <> show val <> "' to Bool"

instance FromSheetValue Text where
  fromSheetValue (SheetString s) = Right s
  fromSheetValue val = Right . pack . show $ val


instance FromSheetValue String where
  fromSheetValue (SheetString s) = Right . unpack $ s
  fromSheetValue val = Right . show $ val

instance FromSheetValue Double where
  fromSheetValue (SheetDouble d) = pure d
  fromSheetValue (SheetString s) = readEither . unpack $ s
  fromSheetValue val = Left $ "Could not parse: '" <> show val <> "' to Double."

instance FromSheetValue Int where
  fromSheetValue (SheetDouble d) = pure $ floor d
  fromSheetValue (SheetString s) = fst <$> decimal s
  fromSheetValue val = Left $ "Could not parse: '" <> show val <> "' to Int."


instance FromSheetValue Integer where
  fromSheetValue (SheetDouble d) = pure $ floor d
  fromSheetValue (SheetString s) = fst <$> decimal s
  fromSheetValue val = Left $ "Could not parse: '" <> show val <> "' to Integer."

