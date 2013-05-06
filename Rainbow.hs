module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

data Color = None | R | O | Y | G | B | V
    deriving (Enum, Eq, Ord, Show)
data Height = One | Two | Three | Four | Five | Six
    deriving (Enum, Eq, Ord, Read, Show)

type Coord = (Int, Int)
type Slot = (Height, Color)
type Board = M.Map Coord Slot

type Solver = StateT (Board, S.Set Slot) []

readPuzzle :: String -> [Height]
readPuzzle s = do
    ls <- lines s
    ws <- words ls
    return $ read ws

heightToSlot :: Height -> Slot
heightToSlot h = (h, None)

makeBoard :: [Height] -> Board
makeBoard hs = at (0, 0) . _Just . _2 .~ R $ M.fromList $
    zip [(i, j) | i <- [0 .. 5], j <- [0 .. 5] ] (map heightToSlot hs)

showBoard :: Board -> String
showBoard b = unlines ls
    where
        ls = map unwords ws
        ws = chunksOf 6 results
        results = map show colors
        colors = map snd . map snd . sort $ M.toList b

initialState :: S.Set Slot
initialState = S.fromList [(h, c) | h <- [One .. Six], c <- [R .. V] ]

colorAt :: Coord -> Solver Color
colorAt c = do
    mval <- use $ _1 . at c
    case mval of
        Just val -> return $ snd val
        Nothing -> StateT (\s -> [])

unique :: Coord -> Solver ()
unique (x, y) = do
    color <- colorAt (x, y)
    forM_ (filter (x /=) [0 .. 5]) $ \x' -> do
        color' <- colorAt (x', y)
        guard $ color /= color'
    forM_ (filter (y /=) [0 .. 5]) $ \y' -> do
        color' <- colorAt (x, y')
        guard $ color /= color'

updateBoard :: Coord -> Slot -> (Board, S.Set Slot) -> (Board, S.Set Slot)
updateBoard c (height, color) board =
    board & _1 . at c . _Just . _2 .~ color & _2 %~ S.delete (height, color)

derp x = traceShow x x

assignCoord :: Coord -> Solver ()
assignCoord c = do
    Just (h, color) <- use $ _1 . at c
    colors <- uses _2 $ S.map snd . S.filter ((h ==) . fst)
    guard $ not (S.null colors)
    board <- use id
    -- Update each part of the state and return them all with a custom StateT
    -- invocation.
    let states = map (\color' -> ((), updateBoard c (h, color') board)) $
                    S.toList colors
    StateT (\_ -> states)
    -- And now ensure that we're not propagating duplicate states.
    unique c

solve :: Solver ()
solve = forM_ [0 .. 5] $ \x ->
    forM_ [0 .. 5] $ \y ->
        assignCoord (x, y)

main :: IO ()
main = do
    input <- getContents
    let puzzle = readPuzzle input
        board = makeBoard puzzle
    print board
    putStr $ showBoard board
    let solved = execStateT solve (board, initialState)
    mapM_ putStr (map (showBoard . fst) solved)
