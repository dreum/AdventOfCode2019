module Main where

import Day1

main :: IO ()
main = do
        contents <- getContents
        let allLines = lines contents
        print $ sum $ fmap (fuelRequired . read) allLines
