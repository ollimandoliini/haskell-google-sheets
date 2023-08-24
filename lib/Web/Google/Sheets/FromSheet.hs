module Web.Google.Sheets.FromSheet where

import Data.Text (Text)
import Data.Vector (Vector, toList)
import Web.Google.Sheets.FromSheetDimension (FromSheetDimension (fromSheetDimension))

class FromSheet a where
  fromSheet :: Vector (Vector Text) -> Either String a

instance (FromSheetDimension a) => FromSheet (Vector a) where
  fromSheet vec = traverse fromSheetDimension vec

instance (FromSheetDimension a) => FromSheet [a] where
  fromSheet vec = traverse fromSheetDimension (toList vec)
