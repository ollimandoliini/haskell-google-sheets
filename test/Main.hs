module Main (main) where

import System.Environment (getEnv)

main :: IO ()
main = do
  env <- getEnv "AUTH_TOKEN"
  print env
  error "sorry"
