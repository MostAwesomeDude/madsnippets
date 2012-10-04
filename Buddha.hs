module Main where

import Control.Monad
import Control.Monad.ST
import Codec.BMP
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString as BS
import Data.Complex
import Data.Function
import Data.Maybe
import Data.Word
import Debug.Trace
import System.Random

data Range a = Range a a a a

data Status = Status { hits, misses :: Int }

hit :: Status -> Status
hit (Status h m) = Status (h + 1) m

miss :: Status -> Status
miss (Status h m) = Status h $ m + 1

totals :: Status -> Int
totals (Status h m) = h + m

palette :: Double -> (Word8, Word8, Word8)
palette x | x > 0.9   = (255,     51,     204) -- violet
          | x > 0.8   = (255,     204,    0) -- gold
          | x > 0.7   = (255,     255,    204) -- silver
          | x > 0.6   = (v,       255,    255) -- greens
          | x > 0.4   = (204,     v + 51, v + 51) -- reds
          | x > 0.2   = (v + 102, v + 51, 102) -- blues
          | x > 0.05  = (v + 51,  v + 51, v + 51) -- grays
          | otherwise = (0,       0,      0) -- black
    where v = floor $ x * 255

invalidRanges :: [Range Double]
invalidRanges = [ Range (-1.2 ) (-1.1 ) 0    0.1
                , Range (-1.1 ) (-0.9 ) 0    0.2
                , Range (-0.9 ) (-0.8 ) 0    0.1
                , Range (-0.69) (-0.61) 0    0.2
                , Range (-0.61) (-0.5 ) 0    0.37
                , Range (-0.5 ) (-0.39) 0    0.48
                , Range (-0.39) ( 0.14) 0    0.55
                , Range ( 0.14) ( 0.29) 0.07 0.42 ]

checkRange :: Complex Double -> Range Double -> Bool
checkRange (i :+ j) (Range i1 i2 j1 j2) =
    not $ i1 < i && i < i2 && j1 < abs j && abs j < j2

checkRanges :: Complex Double -> Bool
checkRanges c = any (checkRange c) invalidRanges

diverged :: Complex Double -> Bool
diverged (i :+ j) = abs i > 1.6 || abs j > 1.6

brot :: Complex Double -> Complex Double -> Complex Double
brot z c = z^2 + c 

iterBrot :: Complex Double -> [Complex Double]
iterBrot c = scanl brot c $ repeat c

brotCount :: Complex Double -> Maybe Int
brotCount c = mi
    where
    mi = if i > 20 && i < 20000 then Just i else Nothing
    i = length brots
    brots = take 20000 $ takeWhile (not . diverged) (iterBrot c)

inc :: STUArray s (Int, Int) Int -> (Int, Int) -> ST s ()
inc a c = do
    e <- readArray a c
    writeArray a c $ e + 1

pointToCoords :: Complex Double -> (Int, Int)
pointToCoords c@(i :+ j) = let
    f :: Double -> Int
    f x = floor $ (i + 1.6) / 3.2 * 800.0
    in (f i, f j)

plotPoint :: 
    STUArray s (Int, Int) Int -> Complex Double -> Int -> ST s ()
plotPoint a c i = let
    brots = map pointToCoords $ take i $ iterBrot c
    in forM_ brots $ inc a

randomC :: StdGen -> (Complex Double, StdGen)
randomC g = let
    (i, g') = randomR (-1.6, 1.6) g
    (j, g'') = randomR (-1.6, 1.6) g'
    in (i :+ j, g'')

maxArray :: UArray (Int, Int) Int -> Double
maxArray = realToFrac . maximum . elems

binnedArray :: UArray (Int, Int) Int -> UArray (Int, Int) Double
binnedArray a = amap (\x -> realToFrac x / maxArray a) a

coloredArray :: UArray (Int, Int) Double -> BS.ByteString
coloredArray a = let
    (_, (x, y)) = bounds a
    f x = case palette x of (r, g, b) -> BS.pack [r, g, b, 0]
    in BS.concat $ map f $ elems a

packBMP :: BS.ByteString -> BMP
packBMP = packRGBA32ToBMP 800 800

arrToBMP :: UArray (Int, Int) Int -> BMP
arrToBMP = packBMP . coloredArray . binnedArray

main :: IO ()
main = do
    gen <- getStdGen
    let arr = runSTUArray $ do
        a <- newArray ((0, 0), (799, 799)) 0
        s <- worker (Status 0 0) a gen
        return a
    writeBMP "test.bmp" $ arrToBMP arr
    where
    worker s a g = let
        (c, g') = randomC g
        mi = brotCount c
        s' = if isJust mi then hit s else miss s
        in do
            case mi of
                Just i -> plotPoint a c i
                Nothing -> return ()
            if (totals s') > 20000
                then return s'
                else worker s' a g'
