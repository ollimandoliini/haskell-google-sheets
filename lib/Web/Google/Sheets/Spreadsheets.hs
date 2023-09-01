{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Sheets.Spreadsheets where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.ByteString (StrictByteString)
import Data.Text (Text)
import Network.HTTP.Req (GET (GET), MonadHttp, NoReqBody (NoReqBody), https, jsonResponse, oAuth2Bearer, req, responseBody, (/:))

-- | https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets#SpreadsheetProperties
data Spreadsheet = Spreadsheet
  { spreadsheetId :: Text
  , properties :: SpreadsheetProperties
  }
  deriving (Show)

instance FromJSON Spreadsheet where
  parseJSON = withObject "Spreadsheet" $ \o ->
    Spreadsheet <$> o .: "spreadsheetId" <*> o .: "properties"

newtype SpreadsheetProperties = SpreadsheetProperties
  { locale :: Text
  }
  deriving (Show)

instance FromJSON SpreadsheetProperties where
  parseJSON = withObject "Spreadsheet" $ \o ->
    SpreadsheetProperties <$> o .: "locale"

getSpreadsheet
  :: (MonadHttp m)
  => StrictByteString
  -- ^ OAuth2 Bearer token
  -> Text
  -- ^ Spreadsheet ID
  -> m Spreadsheet
getSpreadsheet
  accessToken
  spreadsheetId =
    let apiUrl =
          https "sheets.googleapis.com"
            /: "v4"
            /: "spreadsheets"
            /: spreadsheetId
        options = oAuth2Bearer accessToken
     in responseBody <$> req GET apiUrl NoReqBody jsonResponse options
