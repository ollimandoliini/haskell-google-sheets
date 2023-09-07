{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.Spreadsheets.Values
  ( getValues
  , getValueRange
  , updateValues
  , appendValues
  )
where

import Control.Monad (void)
import Data.Aeson (KeyValue ((.=)), object)
import Data.ByteString (StrictByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req
  ( GET (GET)
  , MonadHttp
  , NoReqBody (..)
  , Option
  , POST (POST)
  , PUT (PUT)
  , QueryParam (queryParam)
  , ReqBodyJson (..)
  , Scheme (Https)
  , header
  , https
  , ignoreResponse
  , jsonResponse
  , oAuth2Bearer
  , req
  , responseBody
  , (/:)
  )
import Web.Google.Sheets.Spreadsheets.Values.FromSheet (FromSheet (fromSheet))
import Web.Google.Sheets.Spreadsheets.Values.ToSheet (ToSheet (toSheet))
import Web.Google.Sheets.Types
  ( DatetimeRenderOption (..)
  , Dimension (..)
  , GetValueParams (..)
  , Range (..)
  , ReadValueRange (values)
  , ValueInputOption (..)
  , ValueRenderOption (..)
  , rangeToText
  )

updateValues
  :: (MonadHttp m, ToSheet a)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Maybe Text
  -- ^ Quota project
  -> Text
  -- ^ Spreadsheet ID
  -> Range
  -- ^ Range
  -> ValueInputOption
  -> a
  -> m ()
updateValues
  accessToken
  quotaProject
  spreadsheetId
  range
  valueInputOption
  values =
    let apiUrl =
          https "sheets.googleapis.com"
            /: "v4"
            /: "spreadsheets"
            /: spreadsheetId
            /: "values"
            /: rangeToText range
        options =
          oAuth2Bearer accessToken
            <> queryParams
            <> maybe mempty (header "x-goog-user-project" . encodeUtf8) quotaProject
        reqBody = ReqBodyJson (object ["values" .= toSheet values])
     in void $ req PUT apiUrl reqBody ignoreResponse options
    where
      queryParams :: Option 'Https
      queryParams = queryParam "valueInputOption" (Just (encodeValueInputOption valueInputOption))

      encodeValueInputOption :: ValueInputOption -> Text
      encodeValueInputOption Raw = "RAW"
      encodeValueInputOption UserEntered = "USER_ENTERED"

getValues
  :: (MonadHttp m, FromSheet a)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Maybe Text
  -- ^ Quota project
  -> Text
  -- ^ Spreadsheet ID
  -> Range
  -- ^ Range
  -> GetValueParams
  -> m (Either String a)
getValues
  accessToken
  quotaProject
  spreadsheetId
  range
  params = fromSheet . values <$> getValueRange accessToken quotaProject spreadsheetId range params

getValueRange
  :: (MonadHttp m)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Maybe Text
  -- ^ Quota project
  -> Text
  -- ^ Spreadsheet ID
  -> Range
  -- ^ Range
  -> GetValueParams
  -> m ReadValueRange
getValueRange
  accessToken
  quotaProject
  spreadsheetId
  range
  params =
    let apiUrl =
          https "sheets.googleapis.com"
            /: "v4"
            /: "spreadsheets"
            /: spreadsheetId
            /: "values"
            /: rangeToText range
        options =
          oAuth2Bearer accessToken
            <> paramsToOption params
            <> maybe mempty (header "x-goog-user-project" . encodeUtf8) quotaProject
     in responseBody <$> req GET apiUrl NoReqBody jsonResponse options
    where
      paramsToOption :: GetValueParams -> Option 'Https
      paramsToOption (GetValueParams majorDimension valueRenderOption datetimeRenderOption) =
        mconcat
          [ (queryParam "majorDimension" . Just . encodeMajorDimension) majorDimension
          , (queryParam "valueRenderOption" . Just . encodeValueRenderOption) valueRenderOption
          , (queryParam "dateTimeRenderOption" . Just . encodeDatetimeRenderOption) datetimeRenderOption
          ]

      encodeMajorDimension :: Dimension -> Text
      encodeMajorDimension Row = "ROWS"
      encodeMajorDimension Column = "COLUMNS"

      encodeValueRenderOption :: ValueRenderOption -> Text
      encodeValueRenderOption FormattedValue = "FORMATTED_VALUE"
      encodeValueRenderOption UnformattedValue = "UNFORMATTED_VALUE"
      encodeValueRenderOption Formula = "FORMULA"

      encodeDatetimeRenderOption :: DatetimeRenderOption -> Text
      encodeDatetimeRenderOption SerialNumber = "SERIAL_NUMBER"
      encodeDatetimeRenderOption FormattedString = "FORMATTED_STRING"

appendValues
  :: (MonadHttp m, ToSheet a)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Maybe Text
  -- ^ Quota project
  -> Text
  -- ^ Spreadsheet ID
  -> Range
  -- ^ Range
  -> ValueInputOption
  -- TODO: InsertDataOption
  -> a
  -> m ()
appendValues
  accessToken
  quotaProject
  spreadsheetId
  range
  valueInputOption
  values =
    let apiUrl =
          https "sheets.googleapis.com"
            /: "v4"
            /: "spreadsheets"
            /: spreadsheetId
            /: "values"
            /: rangeToText range
              <> ":append"
        options =
          oAuth2Bearer accessToken
            <> queryParams
            <> maybe mempty (header "x-goog-user-project" . encodeUtf8) quotaProject
        reqBody = ReqBodyJson (object ["values" .= toSheet values])
     in void $ req POST apiUrl reqBody ignoreResponse options
    where
      queryParams :: Option 'Https
      queryParams = queryParam "valueInputOption" (Just (encodeValueInputOption valueInputOption))

      encodeValueInputOption :: ValueInputOption -> Text
      encodeValueInputOption Raw = "RAW"
      encodeValueInputOption UserEntered = "USER_ENTERED"
