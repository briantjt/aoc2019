module Lib where

import System.Directory

fileContents :: String -> IO String
fileContents path = do
  root <- getCurrentDirectory
  readFile (root ++ path)