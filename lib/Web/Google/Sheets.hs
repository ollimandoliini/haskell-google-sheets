-- | A library for interacting with the Google Sheets API.
module Web.Google.Sheets
  ( -- * Values
      module Web.Google.Sheets.Spreadsheets.Values

    -- * Re-exports
  , MonadHttp (..)
  , runReq
  , defaultHttpConfig
  )
where

import Network.HTTP.Req (MonadHttp (..), defaultHttpConfig, runReq)
import Web.Google.Sheets.Spreadsheets.Values
  ( DatetimeRenderOption (..)
  , Dimension
  , GetValueParams (..)
  , Range (..)
  , ReadValueRange (..)
  , SheetRange (..)
  , SheetValue (..)
  , ValueInputOption (..)
  , appendValues
  , clearValues
  , defaultGetValueParams
  , getValueRange
  , getValues
  , updateValues
  )
