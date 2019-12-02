module Main where

import Day1

main :: IO ()
main = do
        contents <- getContents
        let allLines = lines contents
        putStrLn "Day 1, part 1 results:"
        print $ sum $ fmap (fuelRequired . read) allLines
        putStrLn "Day 1, part 2 results:"
        print $ sum $ fmap (totalFuelRequired . read) allLines
