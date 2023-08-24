{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.Spreadsheets.Values
  ( getValueRange
  , getValue
  , getCellValue
  , updateValueRange
  , updateCellValue
  , appendValueRange
  )
where

import Control.Monad (void)
import Data.ByteString (StrictByteString)
import Data.Text (Text, pack)
import Data.Vector qualified as Vector
import Network.HTTP.Req (GET (GET), MonadHttp, NoReqBody (..), Option, POST (POST), PUT (PUT), QueryParam (queryParam), ReqBodyJson (..), Scheme (Https), https, ignoreResponse, jsonResponse, oAuth2Bearer, req, responseBody, (/:))
import Web.Google.Sheets.FromSheet (FromSheet (fromSheet))
import Web.Google.Sheets.Types
  ( Cell (..)
  , DatetimeRenderOption (..)
  , Dimension (..)
  , Range (..)
  , ValueInputOption (..)
  , ValueRange (values)
  , ValueRangeParams (..)
  , ValueRenderOption (..)
  )

updateValueRange
  :: (MonadHttp m)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Text
  -- ^ Spreadsheet ID
  -> Text
  -- ^ Range
  -> Maybe Text
  -- ^ Spreadsheet name
  -> ValueInputOption
  -> ValueRange
  -> m ()
updateValueRange
  accessToken
  spreadsheetId
  range
  mSheetName
  valueInputOption
  valueRange =
    let range' = maybe range (<> "!" <> range) mSheetName
        apiUrl =
          https "sheets.googleapis.com"
            /: "v4"
            /: "spreadsheets"
            /: spreadsheetId
            /: "values"
            /: range'
        options = oAuth2Bearer accessToken <> queryParams
     in void $ req PUT apiUrl (ReqBodyJson valueRange) ignoreResponse options
    where
      queryParams :: Option 'Https
      queryParams = queryParam "valueInputOption" (Just (encodeValueInputOption valueInputOption))

      encodeValueInputOption :: ValueInputOption -> Text
      encodeValueInputOption Raw = "RAW"
      encodeValueInputOption UserEntered = "USER_ENTERED"

updateCellValue
  :: (MonadHttp m)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Text
  -- ^ Spreadsheet ID
  -> Cell
  -- ^ Cell
  -> Maybe Text
  -- ^ Sheet name
  -> ValueInputOption
  -> ValueRange
  -> m ()
updateCellValue accessToken spreadSheetId cell sheetName params =
  updateValueRange accessToken spreadSheetId (cellToRange cell) sheetName params

getValueRange
  :: (MonadHttp m)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Text
  -- ^ Spreadsheet ID
  -> Range
  -> ValueRangeParams
  -- ^ Value
  -> m ValueRange
getValueRange
  accessToken
  spreadsheetId
  range
  params =
    let range' =
          case range of
            RangeDefaultSheet r -> r
            RangeWithSheet s r -> s <> "!" <> r
            FullSheet s -> s
        apiUrl =
          https "sheets.googleapis.com"
            /: "v4"
            /: "spreadsheets"
            /: spreadsheetId
            /: "values"
            /: range'
        options = oAuth2Bearer accessToken <> paramsToOption params
     in responseBody <$> req GET apiUrl NoReqBody jsonResponse options
    where
      paramsToOption :: ValueRangeParams -> Option 'Https
      paramsToOption (ValueRangeParams majorDimension valueRenderOption datetimeRenderOption) =
        mconcat
          [ maybe mempty (queryParam "majorDimension" . Just . encodeMajorDimension) majorDimension
          , maybe mempty (queryParam "valueRenderOption" . Just . encodeValueRenderOption) valueRenderOption
          , maybe mempty (queryParam "dateTimeRenderOption" . Just . encodeDatetimeRenderOption) datetimeRenderOption
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

getValue
  :: (MonadHttp m, FromSheet a)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Text
  -- ^ Spreadsheet ID
  -> Range
  -- ^ Range
  -> ValueRangeParams
  -> m (Either String a)
getValue accessToken spreadSheetId range valueRangeParams =
  fromSheet . values <$> getValueRange accessToken spreadSheetId range valueRangeParams

getCellValue
  :: (MonadHttp m)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Text
  -- ^ Spreadsheet ID
  -> Cell
  -- ^ Cell
  -> Maybe Text
  -- ^ Sheet name
  -> ValueRangeParams -- TODO: Consider this naming
  -> m Text
getCellValue accessToken spreadSheetId cell mSheetName params =
  let range = case mSheetName of
        Just sheetName -> RangeWithSheet (cellToRange cell) sheetName
        Nothing -> RangeDefaultSheet (cellToRange cell)
   in Vector.head . Vector.head . values <$> getValueRange accessToken spreadSheetId range params

appendValueRange
  :: (MonadHttp m)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Text
  -- ^ Spreadsheet ID
  -> Text
  -- ^ Range
  -> Maybe Text
  -- ^ Sheet name
  -> ValueInputOption
  -> ValueRange
  -> m ()
appendValueRange
  accessToken
  spreadsheetId
  range
  mSheetName
  valueInputOption
  valueRange =
    let range' = maybe range (<> "!" <> range) mSheetName
        apiUrl =
          https "sheets.googleapis.com"
            /: "v4"
            /: "spreadsheets"
            /: spreadsheetId
            /: "values"
            /: range'
            /: ":append"
        options = oAuth2Bearer accessToken <> queryParams
     in void $ req POST apiUrl (ReqBodyJson valueRange) ignoreResponse options
    where
      queryParams :: Option 'Https
      queryParams =
        queryParam "valueInputOption"
          $ Just
          $ case valueInputOption of
            Raw -> "RAW" :: Text
            UserEntered -> "USER_ENTERED"

cellToRange :: Cell -> Text
cellToRange (Cell row column) =
  enumerateColumns
    !! fromIntegral column
    <> (pack . show $ (row + 1))
    <> ":"
    <> enumerateColumns
    !! fromIntegral column
    <> (pack . show $ (row + 1))
  where
    enumerateColumns :: [Text]
    enumerateColumns = pack <$> go letters
      where
        letters = (: []) <$> ['A' .. 'Z']
        go xs = xs ++ go [a : b | a <- ['A' .. 'Z'], b <- xs]
