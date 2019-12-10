module Day9.Day9 where

import Lib
import IntCode
import qualified Data.Map.Strict as M
import Control.Concurrent.Chan

day9Part1 :: IO ()
day9Part1 = do
  contents <- fileContents "/src/Day9/input.txt"
  inChan <- newChan
  outChan <- newChan
  writeChan inChan (1 :: Integer)
  let values = parseInput contents
      ram = M.fromList (zip [0..] values)
  processOpCode ram 0 0 inChan outChan
  readChan outChan >>= print . show

day9Part2 :: IO ()
day9Part2 = do
  contents <- fileContents "/src/Day9/input.txt"
  inChan <- newChan
  outChan <- newChan
  writeChan inChan (2 :: Integer)
  let values = parseInput contents
      ram = M.fromList (zip [0..] values)
  processOpCode ram 0 0 inChan outChan
  readChan outChan >>= print . show