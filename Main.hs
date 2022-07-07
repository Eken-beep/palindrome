module Main where

import Data.List
import Data.Char

reverser i = map reverse $ words i

dupes l                                    = dupes' l []
  where
    dupes' []     ls                       = ls
    dupes' (x:xs) ls
        | not (x `elem` ls) && x `elem` xs = dupes' xs (x:ls)
        | otherwise                        = dupes' xs ls

main :: IO ()
main = do
    contents <- readFile "english_words.txt"
    let allWords = reverser contents ++ words contents

    print $ dupes allWords
