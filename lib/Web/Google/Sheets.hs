-- | A library for interacting with the Google Sheets API.
module Web.Google.Sheets
  ( -- * Values
    getValues
  , getValueRange
  , updateValues
  , appendValues
  , SheetRange (..)
  , Range (..)
  , GetValueParams (..)
  , defaultGetValueParams
  , Dimension
  , ValueInputOption (..)
  , DatetimeRenderOption (..)
  , ReadValueRange

    -- * Re-exports
  , MonadHttp (..)
  , runReq
  , defaultHttpConfig
  )
where

import Network.HTTP.Req (MonadHttp (..), defaultHttpConfig, runReq)
import Web.Google.Sheets.Spreadsheets.Values
  ( appendValues
  , getValueRange
  , getValues
  , updateValues
  )
import Web.Google.Sheets.Spreadsheets.Values.Types
  ( DatetimeRenderOption (..)
  , Dimension
  , GetValueParams (..)
  , Range (..)
  , ReadValueRange
  , SheetRange (..)
  , ValueInputOption (..)
  , defaultGetValueParams
  )
