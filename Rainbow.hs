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

import Debug.Trace

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
        results = map showPair colors
        colors = map snd . sort $ M.toList b
        showPair (h, c) = show (fromEnum h) ++ show c

initialState :: [Slot]
initialState = [(h, c) | h <- [One .. Six], c <- [R .. V]]

colorAt :: Coord -> Solver Color
colorAt c = do
    mval <- use $ _1 . at c
    case mval of
        Just val -> return $ snd val
        Nothing -> StateT (const [])

goodRow :: [Color] -> Bool
goodRow cs = all pred $ zip cs' (tail cs')
    where
    cs' = sort cs
    pred (None, None) = True
    pred (x, y) = x /= y

unique :: Coord -> Solver ()
unique (x, y) = do
    cs <- forM [0 .. 5] $ \y' -> colorAt (x, y')
    guard $ goodRow cs
    cs' <- forM [0 .. 5] $ \x' -> colorAt (x', y)
    guard $ goodRow cs'
    -- zoom _1 $ modify (\b -> trace (showBoard b) b)

assignCoord :: Coord -> Solver ()
assignCoord c = do
    (map, colors) <- get
    let Just (h, color) = map ^. at c
    -- Pull out the different possibilities.
    ((h', color'), colors') <- lift $ withEach colors
    -- Guard away the ones that we don't like.
    guard $ h == h'
    -- Update the state.
    put (M.insert c (h, color') map, colors')
    -- And now ensure that we're not propagating duplicate states.
    unique c

solve :: Solver ()
solve = forM_ [0 .. 5] $ \x ->
    forM_ [0 .. 5] $ \y ->
        traceShow (x, y) $ assignCoord (x, y)

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
