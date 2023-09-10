# Haskell Google Sheets

A Haskell library for interacting with the Google Sheets API.

## Usage example

```hs

import Web.Google.Sheets

main :: IO ()
main = do
    let token = "..."
        range = RangeWithSheetName (Just (FullRange 0 0 2 0)) sheetName
        spreadsheetId = "..."
    value :: Either String [[Text]] <- runReq defaultHttpConfig (getValues token Nothing spreadsheetId range defaultGetValueParams)
    print value
```

## Todo

- [x] Fix parser for `values`
- [x] Switch to using Natural
- [ ] Write README and CHANGELOG
- [ ] Write examples
- [x] Finish documentation
- [x] Proof read documentation
- [ ] Finish tests
- [ ] InsertDataOption
- [x] Consider location of `Types` module
- [x] Check contents of `Types` module
- [ ] Write tests for `FromSheet` instances
