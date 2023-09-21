export AUTH_TOKEN=$(gcloud auth application-default print-access-token)
export QUOTA_PROJECT="things-394312"
export SPREADSHEET_ID="1PHUwechIwIa2lyw522dUbsvdVgh-6-eJSz_jVXuAKgw"
export SHEET_NAME="ghc-9.6.2"
cabal test
