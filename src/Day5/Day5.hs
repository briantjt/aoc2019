module Day5.Day5 where

import           System.Directory
import           Data.List.Split
import           Data.Array.IO
import           Day4.Day4                      ( splitDigits )
import           Control.Concurrent.Chan

day5Part1 :: IO ()
day5Part1 = do
  root  <- getCurrentDirectory
  input <- readFile (root ++ "/src/Day5/input.txt")
  let values = map (\x -> read x :: Int) $ splitOn "," input
  array   <- newListArray (0, length values - 1) values :: IO (IOArray Int Int)
  channel <- newChan
  writeChan channel 5
  processOpCode array 0 channel channel
  readChan channel >>= print
  return ()

parseInput :: String -> [Int]
parseInput = map (\x -> read x :: Int) . splitOn ","

splitInstruction :: Int -> [Int]
splitInstruction i =
  let (quotient, opCode) = i `quotRem` 100 in splitDigits quotient ++ [opCode]

processOpCode
  :: IOArray Int Int -> Int -> Chan Int -> Chan Int -> IO (IOArray Int Int)
processOpCode array pos inputChannel outputChannel = do
  operation : modes <- reverse . splitInstruction <$> readArray array pos
  case operation of
    99 -> return array
    3  -> do
      val        <- readChan inputChannel
      posToWrite <- readArray array (pos + 1)
      writeArray array posToWrite val
      processOpCode array (pos + 2) inputChannel outputChannel
    4 -> do
      param <- readArray array (pos + 1)
      let mode = if null modes then 0 else head modes
      val <- if mode == 1 then return param else readArray array param
      writeChan outputChannel val
      processOpCode array (pos + 2) inputChannel outputChannel
    _ -> do
      param1 <- readArray array (pos + 1)
      param2 <- readArray array (pos + 2)
      let mode1 = if null modes then 0 else head modes
          mode2 = if length modes < 2 then 0 else modes !! 1
      i1      <- if mode1 == 1 then return param1 else readArray array param1

      i2      <- if mode2 == 1 then return param2 else readArray array param2
      nextPos <- readArray array (pos + 3)
      case operation of
        1 -> do
          writeArray array nextPos (i1 + i2)
          processOpCode array (pos + 4) inputChannel outputChannel
        2 -> do
          writeArray array nextPos (i1 * i2)
          processOpCode array (pos + 4) inputChannel outputChannel
        5 -> if i1 /= 0
          then processOpCode array i2 inputChannel outputChannel
          else processOpCode array (pos + 3) inputChannel outputChannel
        6 -> if i1 == 0
          then processOpCode array i2 inputChannel outputChannel
          else processOpCode array (pos + 3) inputChannel outputChannel
        7 -> if i1 < i2
          then
            writeArray array nextPos 1
              >> processOpCode array (pos + 4) inputChannel outputChannel
          else
            writeArray array nextPos 0
              >> processOpCode array (pos + 4) inputChannel outputChannel
        8 -> if i1 == i2
          then
            writeArray array nextPos 1
              >> processOpCode array (pos + 4) inputChannel outputChannel
          else
            writeArray array nextPos 0
              >> processOpCode array (pos + 4) inputChannel outputChannel
        _ -> return array
