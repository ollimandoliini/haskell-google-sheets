{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.Spreadsheets.Values
  ( getValues
  , getValueRange
  , updateValues
  , appendValues
  , clearValues
  , module Web.Google.Sheets.Spreadsheets.Values.Types
  )
where

import Control.Monad (void)
import Data.Aeson (KeyValue ((.=)), object)
import Data.ByteString (StrictByteString)
import Data.Text (Text, pack)
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
import Numeric.Natural (Natural)
import Web.Google.Sheets.Spreadsheets.Values.FromSheet (FromSheet (fromSheet))
import Web.Google.Sheets.Spreadsheets.Values.ToSheet (ToSheet (toSheet))
import Web.Google.Sheets.Spreadsheets.Values.Types
  ( DatetimeRenderOption (..)
  , Dimension (..)
  , GetValueParams (..)
  , InsertDataOption (..)
  , Range (..)
  , ReadValueRange (..)
  , SheetRange (..)
  , SheetValue (..)
  , ValueInputOption (..)
  , ValueRenderOption (..)
  , defaultGetValueParams
  )

-- | Update values on a range.
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

-- | Query and decode values from a sheet.
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

-- | A version of `Web.Google.Sheets.Spreadsheets.Values.getValues` that doesn't do decoding.
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

-- | Append values to a range.
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
  -> InsertDataOption
  -> a
  -> m ()
appendValues
  accessToken
  quotaProject
  spreadsheetId
  range
  valueInputOption
  insertDataOption
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
      queryParams =
        queryParam "valueInputOption" (Just (encodeValueInputOption valueInputOption))
          <> queryParam "insertDataOption" (Just (encodeInsertDataOption insertDataOption))

      encodeValueInputOption :: ValueInputOption -> Text
      encodeValueInputOption Raw = "RAW"
      encodeValueInputOption UserEntered = "USER_ENTERED"

      encodeInsertDataOption :: InsertDataOption -> Text
      encodeInsertDataOption Overwrite = "OVERWRITE"
      encodeInsertDataOption InsertRows = "INSERT_ROWS"

-- | Clear values from a range.
clearValues
  :: (MonadHttp m)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Maybe Text
  -- ^ Quota project
  -> Text
  -- ^ Spreadsheet ID
  -> Range
  -- ^ Range
  -> m ()
clearValues
  accessToken
  quotaProject
  spreadsheetId
  range =
    let apiUrl =
          https "sheets.googleapis.com"
            /: "v4"
            /: "spreadsheets"
            /: spreadsheetId
            /: "values"
            /: rangeToText range
            <> ":clear"
        options =
          oAuth2Bearer accessToken
            <> maybe mempty (header "x-goog-user-project" . encodeUtf8) quotaProject
     in void $ req POST apiUrl NoReqBody ignoreResponse options

coordinatesToRange :: Natural -> Natural -> Text
coordinatesToRange column row =
  enumerateColumns
    !! fromIntegral column
    <> (pack . show $ (row + 1))

columnToText :: Natural -> Text
columnToText column = enumerateColumns !! fromIntegral column

rowToText :: Natural -> Text
rowToText = pack . show . (+ 1)

-- | *NOTE* Produces an infinite list
enumerateColumns :: [Text]
enumerateColumns = pack <$> go letters
  where
    letters = (: []) <$> ['A' .. 'Z']
    go xs = xs ++ go [a : b | a <- ['A' .. 'Z'], b <- xs]

rangeToText :: Range -> Text
rangeToText (RangeWithDefaultSheet sheetRange) = sheetRangeToText sheetRange
rangeToText (RangeWithSheetName (Just sheetRange) sheetName) = "'" <> sheetName <> "'!" <> sheetRangeToText sheetRange
rangeToText (RangeWithSheetName Nothing sheetName) = sheetName

sheetRangeToText :: SheetRange -> Text
sheetRangeToText (FullRange startColumn startRow endColumn endRow) =
  coordinatesToRange startColumn startRow <> ":" <> coordinatesToRange endColumn endRow
sheetRangeToText (RowRange startRow endRow) = rowToText startRow <> ":" <> rowToText endRow
sheetRangeToText (ColumnRange startColumn endColumn) = columnToText startColumn <> ":" <> columnToText endColumn
sheetRangeToText (PartialColumnRange startColumn mStartRow endColumn mEndRow) =
  columnToText startColumn <> maybe "" rowToText mStartRow <> ":" <> columnToText endColumn <> maybe "" rowToText mEndRow
