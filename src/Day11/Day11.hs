{-# LANGUAGE TupleSections #-}

module Day11.Day11 where

import           Lib
import           IntCode
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import qualified Data.Map.Strict               as M
import           Text.Printf
import           Data.Maybe
import           Data.List
import           Control.Monad

data Direction = Lef | Down | Righ | Up deriving Show

nextDirection :: Direction -> Integer -> Direction
nextDirection d turn = case d of
  Lef  -> if turn == 0 then Down else Up
  Down -> if turn == 0 then Righ else Lef
  Righ -> if turn == 0 then Up else Down
  Up   -> if turn == 0 then Lef else Righ

nextCoordinates :: (Integer, Integer) -> Direction -> (Integer, Integer)
nextCoordinates (x, y) d = case d of
  Lef  -> (x - 1, y)
  Down -> (x, y - 1)
  Righ -> (x + 1, y)
  Up   -> (x, y + 1)

readOutput :: Chan Integer -> IO (Integer, Integer)
readOutput ch = do
  color <- readChan ch
  dir   <- readChan ch
  return (color, dir)

day11 :: IO ()
day11 = do
  contents <- fileContents "/src/Day11/input.txt"
  inChan   <- newChan
  outChan  <- newChan
  -- Change to 0 for part 1
  writeChan inChan (1 :: Integer)
  let values    = parseInput contents
      ram       = M.fromList (zip [0 ..] values)
      positions = M.empty
  t1 <- async $ processOpCode ram 0 0 inChan outChan
  let loop pos dir grid = do
        t2     <- async $ readOutput outChan
        output <- waitEither t1 t2
        case output of
          (Right (color, turn)) -> do
            let nextGrid = M.insert pos color grid
                nextDir  = nextDirection dir turn
                nextPos  = nextCoordinates pos nextDir
            writeChan inChan (fromMaybe 0 (M.lookup nextPos nextGrid))
            loop nextPos nextDir nextGrid
          (Left _) -> do
            (color, _) <- wait t2
            return $ M.insert pos color grid
  image <- loop (0, 0) Up positions
  -- print length image -- part 1
  let coordinates = M.keys image
      getBounds = sequenceA [minimum, maximum]
      [minX, maxX]        = getBounds $ map fst coordinates
      [minY, maxY]        = getBounds $ map snd coordinates
      array = map (\y -> map (, y) [minX .. maxX]) [maxY, (maxY - 1) .. minY]
  forM_
    [maxY, (maxY - 1) .. minY]
    (\y ->
      forM_
          [minX .. maxX]
          (\x -> putStr
            (if fromMaybe 0 (M.lookup (x, y) image) == 0 then " " else "$")
          )
        >> putStrLn ""
    )

test :: IO ()
test = do
  contents <- fileContents "/src/Day11/example2.txt"
  inChan   <- newChan
  outChan  <- newChan
  writeChan inChan (5 :: Integer)
  let values = parseInput contents
      ram    = M.fromList (zip [0 ..] values)
  processOpCode ram 0 0 inChan outChan
  readChan outChan >>= print
