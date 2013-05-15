module Main where

-- A solver for the "Rainbow" 36 Cube puzzle.
-- I got inspired after seeing one at work and being confident that a simple
-- solution would be searchable. I also needed to buff up my Haskell chops.

import Control.Lens
import Control.Monad
import Control.Monad.State
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

type Solver = StateT (Board, [Slot]) []

-- This particular form comes from Cale, because Cale is awesome.
withEach :: [a] -> [(a, [a])]
withEach [] = []
withEach (a:as) = (a, as) : [(b, a : bs) | (b, bs) <- withEach as]

readPuzzle :: String -> [Height]
readPuzzle s = do
    ls <- lines s
    ws <- words ls
    return $ read ws

heightToSlot :: Height -> Slot
heightToSlot h = (h, None)

makeBoard :: [Height] -> Board
makeBoard hs = M.fromList $
    zip [(i, j) | i <- [0 .. 5], j <- [0 .. 5] ] (map heightToSlot hs)

showBoard :: Board -> String
showBoard b = unlines ls
    where
        ls = map unwords ws
        ws = chunksOf 6 results
        results = map show colors
        colors = map (snd . snd) . sort $ M.toList b

initialState :: [Slot]
initialState = [(h, c) | h <- [One .. Six], c <- [R .. V]]

colorAt :: Coord -> Solver Color
colorAt c = do
    mval <- use $ _1 . at c
    case mval of
        Just val -> return $ snd val
        Nothing -> StateT (const [])

unique :: Coord -> Solver ()
unique (x, y) = do
    color <- colorAt (x, y)
    forM_ (filter (x /=) [0 .. 5]) $ \x' -> do
        color' <- colorAt (x', y)
        guard $ color /= color'
    forM_ (filter (y /=) [0 .. 5]) $ \y' -> do
        color' <- colorAt (x, y')
        guard $ color /= color'

assignCoord :: Coord -> Solver ()
assignCoord c = do
    Just (h, color) <- use $ _1 . at c
    colors <- use _2
    -- Pull out the different possibilities.
    ((h', color'), colors') <- lift $ withEach colors
    -- Guard away the ones that we don't like.
    guard $ h == h'
    -- Update the state.
    _1 . at c . _Just . _2 .= color'
    _2 .= colors'
    -- And now ensure that we're not propagating duplicate states.
    -- unique c

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
    case solved of
        solution:_ -> do
            putStr $ "Solution:\n"
            putStr . showBoard . fst $ solution
        [] -> putStr "No solutions."
