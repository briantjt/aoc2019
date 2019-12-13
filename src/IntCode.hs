module IntCode where

import           Data.List.Split
import           Data.Maybe
import           Control.Concurrent.Chan
import qualified Data.Map.Strict               as M
import           Debug.Trace
import           Text.Printf
import           System.IO

type RAM = M.Map Integer Integer
type MemPointer = Integer
type InstructionPointer = Integer

splitDigits :: Integer -> [Integer]
splitDigits i | i < 10    = [i]
              | otherwise = splitDigits (i `quot` 10) ++ [i `mod` 10]

parseInput :: String -> [Integer]
parseInput = map (\x -> read x :: Integer) . splitOn ","

splitInstruction :: Integer -> [Integer]
splitInstruction i =
  let (modes, opCode) = i `quotRem` 100
  in  [opCode] ++ reverse (splitDigits modes) ++ repeat 0

opArgs :: [Integer]
opArgs = [0, 3, 3, 1, 1, 2, 2, 3, 3, 1]

processOpCode
  :: RAM
  -> InstructionPointer
  -> MemPointer
  -> Chan Integer
  -> Chan Integer
  -> IO RAM
processOpCode ram intPointer memPointer inputChannel outputChannel = do
  let operation : modes =
        fromJust (splitInstruction <$> M.lookup intPointer ram)
  if operation == 99
    then return ram
    else do
      let argsRequired = opArgs !! fromIntegral operation
          args = map (\pos -> fromJust $ M.lookup (intPointer + pos) ram)
                     [1 .. argsRequired]
          argModes = zip args modes
      let readValues = map
            (\(arg, mode) -> case mode of
              0 -> M.lookup arg ram
              1 -> Just arg
              2 -> M.lookup (arg + memPointer) ram
            )
            argModes
      let writeValues = map
            (\(arg, mode) -> case mode of
              0 -> arg
              1 -> 0
              2 -> arg + memPointer
            )
            argModes
      case operation of
        1 -> processOpCode
          (M.insert (writeValues !! 2)
                    (sum $ take 2 (fromMaybe 0 <$> readValues))
                    ram
          )
          (intPointer + 4)
          memPointer
          inputChannel
          outputChannel
        2 -> processOpCode
          (M.insert (writeValues !! 2)
                    (product $ take 2 (fromMaybe 0 <$> readValues))
                    ram
          )
          (intPointer + 4)
          memPointer
          inputChannel
          outputChannel
        3 -> do
          val <- readChan inputChannel
          processOpCode (M.insert (head writeValues) val ram)
                        (intPointer + 2)
                        memPointer
                        inputChannel
                        outputChannel
        4 -> do
          writeChan outputChannel (fromJust $ head readValues)
          processOpCode ram
                        (intPointer + 2)
                        memPointer
                        inputChannel
                        outputChannel
        5 -> processOpCode
          ram
          (if (fromJust $ head readValues) /= 0
            then (fromJust $ readValues !! 1)
            else intPointer + 3
          )
          memPointer
          inputChannel
          outputChannel
        6 -> processOpCode
          ram
          (if (fromJust $ head readValues) == 0
            then (fromJust $ readValues !! 1)
            else intPointer + 3
          )
          memPointer
          inputChannel
          outputChannel
        7 -> processOpCode
          (M.insert (writeValues !! 2)
                    (if head readValues < (readValues !! 1) then 1 else 0)
                    ram
          )
          (intPointer + 4)
          memPointer
          inputChannel
          outputChannel
        8 -> processOpCode
          (M.insert (writeValues !! 2)
                    (if head readValues == (readValues !! 1) then 1 else 0)
                    ram
          )
          (intPointer + 4)
          memPointer
          inputChannel
          outputChannel
        9 -> processOpCode ram
                           (intPointer + 2)
                           (memPointer + (fromJust $ head readValues))
                           inputChannel
                           outputChannel
        _ -> print "ERROR" >> (print $ M.toList ram) >> return ram

