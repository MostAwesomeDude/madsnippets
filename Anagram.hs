module Main where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import System.Environment

type Anagrams = M.Map String [String]

normalize :: String -> String
normalize = sort . map toLower

addWord :: String -> Anagrams -> Anagrams
addWord s m = let
    k = normalize s
    ss = M.findWithDefault [] k m
    in M.insert k (s:ss) m

anagrams :: [String] -> Anagrams
anagrams = foldr addWord M.empty

findWords :: String -> Anagrams -> [String]
findWords s = M.findWithDefault [] $ normalize s

makeAnagrams :: String -> Anagrams
makeAnagrams = anagrams . lines

main :: IO ()
main = do
    stuff <- readFile "/usr/share/dict/words"
    targets <- getArgs
    let m = anagrams $ lines stuff
    forM_ targets $ \word ->
        putStrLn $ "Found words: " ++ show (findWords word m)
