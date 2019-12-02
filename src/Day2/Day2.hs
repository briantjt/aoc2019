module Day2.Day2 where

import           System.Directory
import           Data.List.Split
import           Data.Array.MArray
import           Data.Array.IO

day2Part1 :: IO Int
day2Part1 = do
  root  <- getCurrentDirectory
  input <- readFile (root ++ "/src/Day2/input.txt")
  let values = map (\x -> read x :: Int) $ splitOn "," input
  array <- newListArray (0, length values - 1) values :: IO (IOArray Int Int)
  writeArray array 1 12
  writeArray array 2 2
  processOpCode array 0
  readArray array 0

day2Part2 = do
  root  <- getCurrentDirectory
  input <- readFile (root ++ "/src/Day2/input.txt")
  let values = map (\x -> read x :: Int) $ splitOn "," input
  values <- mapM
    (\(a, b) -> do
      array  <- newListArray (0, length values - 1) values :: IO (IOArray Int Int)
      writeArray array 1 a
      writeArray array 2 b
      processOpCode array 0
      ans <- readArray array 0
      if ans == 19690720 then return (a, b) else return (-1, -1)
    )
    [ (a, b) | a <- [0 .. 99], b <- [0 .. 99] ]

  return $ head $ filter (\(a, b) -> a > -1) values

processOpCode :: IOArray Int Int -> Int -> IO (IOArray Int Int)
processOpCode array pos = do
  instruction <- readArray array pos
  if instruction == 99
    then return array
    else do
      pos1    <- readArray array (pos + 1)
      pos2    <- readArray array (pos + 2)
      i1      <- readArray array pos1
      i2      <- readArray array pos2
      nextPos <- readArray array (pos + 3)
      if instruction == 1
        then do
          writeArray array nextPos (i1 + i2)
          processOpCode array (pos + 4)
        else if instruction == 2
          then do
            writeArray array nextPos (i1 * i2)
            processOpCode array (pos + 4)
          else return array
