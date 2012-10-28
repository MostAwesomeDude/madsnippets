module Main where

import Data.Char
import Data.List
import qualified Data.Map as M

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

main :: IO ()
main = do
    stuff <- readFile "/usr/share/dict/words"
    let m = anagrams $ lines stuff
        found = M.findWithDefault [] (normalize "traps") m
    putStrLn $ "Found words: " ++ show found
