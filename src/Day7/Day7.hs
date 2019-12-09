module Day7.Day7 where

import           Day5.Day5
import           Lib
import           Data.List
import           Data.Array.IO
import           Control.Concurrent.Chan
import Control.Concurrent.Async
import           Control.Monad

day7Part1 :: IO ()
day7Part1 = do
  contents <- fileContents "/src/Day7/input.txt"
  let values = parseInput contents
      inputs = permutations [0 .. 4]
  outputs <- mapM (\ss -> newChan >>= \ch -> processSequence ss values ch 0)
                  inputs
  print $ maximum outputs

processSequence :: [Int] -> [Int] -> Chan Int -> Int -> IO Int
processSequence [] _       _            i = return i
processSequence s  program inputChannel i = do
  array <- newListArray (0, length program - 1) program :: IO (IOArray Int Int)
  writeChan inputChannel (head s)
  writeChan inputChannel i
  processOpCode array 0 inputChannel inputChannel
  readChan inputChannel >>= processSequence (tail s) program inputChannel

day7Part2 :: IO ()
day7Part2 = do
  contents <- fileContents "/src/Day7/input.txt"
  let values = parseInput contents
      inputs = permutations [5 .. 9]
  outputs <- mapM
    (\ss -> do
      channels <- replicateM 5 newChan
      zipWithM_ writeChan channels ss
      writeChan (head channels) 0
      arrays <- replicateM
        5
        (newListArray (0, length values - 1) values :: IO (IOArray Int Int))
      let [chan1, chan2, chan3, chan4, chan5] = channels
          [a1   , a2   , a3   , a4   , a5   ] = arrays
      t1 <- async (processOpCode a1 0 chan1 chan2)
      t2 <- async (processOpCode a2 0 chan2 chan3)
      t3 <- async (processOpCode a3 0 chan3 chan4)
      t4 <- async (processOpCode a4 0 chan4 chan5)
      t5 <- async (processOpCode a5 0 chan5 chan1)
      mapM_ wait [t1, t2, t3, t4, t5]
      readChan chan1
    )
    inputs
  print $ maximum outputs
