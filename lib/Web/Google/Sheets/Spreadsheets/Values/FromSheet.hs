{-# LANGUAGE OverloadedStrings #-}

-- | This module introduces a hierachy of type classes for converting sheet values into Haskell values.
module Web.Google.Sheets.Spreadsheets.Values.FromSheet where

import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import Data.Vector (Vector, toList)
import Text.Read (readEither)
import Web.Google.Sheets.Spreadsheets.Values.Types (ReadSheetValue(..))


-- | `FromSheet` is the highest level of the decoding type class hiearchy. It is used to convert a two-dimensional
-- (a vector of vectors) value into some value `a`.
class FromSheet a where
  fromSheet :: Vector (Vector ReadSheetValue) -> Either String a

instance (FromSheetDimension a) => FromSheet (Vector a) where
  fromSheet vec = traverse fromSheetDimension vec

instance (FromSheetDimension a) => FromSheet [a] where
  fromSheet vec = traverse fromSheetDimension (toList vec)

-- | `FromSheetDimension` is the second level of the decoding type class hiearchy. It is used to convert a one-dimensional
-- (a vectors) value into some value @a@. Most commonly this type class is used when you sheet contains uniform records on
-- rows or columns. In such case you might want to define an instance of @FromSheetDimension MyType@ and then you can
-- use `getValues` to retrieve @Vector MyType@ from a sheet.
class FromSheetDimension a where
  fromSheetDimension :: Vector ReadSheetValue -> Either String a

instance (FromSheetValue a) => FromSheetDimension (Vector a) where
  fromSheetDimension vec = traverse fromSheetValue vec

instance (FromSheetValue a) => FromSheetDimension [a] where
  fromSheetDimension vec = toList <$> traverse fromSheetValue vec

-- | `FromSheetValue` is the third and the lowest level of the decoding type class hiearchy. It is used to convert a single
-- value into some value @a@.
class FromSheetValue a where
  fromSheetValue :: ReadSheetValue -> Either String a

instance FromSheetValue ReadSheetValue where
  fromSheetValue = pure

instance FromSheetValue Bool where
  fromSheetValue (ReadSheetValue "TRUE") = Right True
  fromSheetValue (ReadSheetValue "FALSE") = Right False
  fromSheetValue val = Left $ "Could not convert '" <> show val <> "' to Bool"

instance FromSheetValue Text where
  fromSheetValue = Right . coerce

instance FromSheetValue String where
  fromSheetValue (ReadSheetValue s) = Right . unpack $ s

instance FromSheetValue Double where
  fromSheetValue (ReadSheetValue s) = readEither . unpack $ s

instance FromSheetValue Float where
  fromSheetValue (ReadSheetValue s) = readEither . unpack $ s

instance FromSheetValue Int where
  fromSheetValue (ReadSheetValue s) = readEither . unpack $ s

instance FromSheetValue Integer where
  fromSheetValue (ReadSheetValue s) = readEither . unpack $ s
