module Main where

import Data.List
import Data.List.Split
import qualified Data.Map as M

data Color = None | R | O | Y | G | B | V
    deriving (Enum, Eq, Ord, Show)
data Height = One | Two | Three | Four | Five | Six
    deriving (Enum, Eq, Ord, Read, Show)

type Coord = (Int, Int)
type Slot = (Height, Color)
type Board = M.Map Coord Slot

readPuzzle :: String -> [Height]
readPuzzle s = do
    ls <- lines s
    ws <- words ls
    return $ read ws

heightToSlot :: Height -> Slot
heightToSlot h = (h, None)

makeBoard :: [Height] -> Board
makeBoard hs = M.fromList $
    zip [(i, j) | i <- [0..6], j <- [0..6] ] (map heightToSlot hs)

showBoard :: Board -> String
showBoard b = unlines ls
    where
        ls = map unwords ws
        ws = chunksOf 6 results
        results = map show colors
        colors = map snd . map snd . sort $ M.toList b

main :: IO ()
main = do
    input <- getContents
    let puzzle = readPuzzle input
        board = makeBoard puzzle
    print board
    putStr $ showBoard board
