module Day4 
    ( hasDuplicate,
      nonDecreasing,
      parseDigits,
      runDay4
    ) where

import Control.Monad

listToPairs :: [a] -> [(a,a)]
listToPairs [] = []
listToPairs (x:[]) = []
listToPairs (x:y:xs) = (x,y) : listToPairs (y:xs)

hasDuplicate :: [Int] -> Bool
hasDuplicate xs = any (uncurry (==)) $ listToPairs xs

nonDecreasing :: [Int] -> Bool
nonDecreasing xs = all (uncurry (<=)) $ listToPairs xs

charToString c = [c]

parseDigits :: Int -> [Int]
parseDigits s = map (read . charToString) $ show s

runDay4  = "Day4 part 1:\n" ++
           (show $ length $ filter (liftM2 (&&) hasDuplicate nonDecreasing) $ map parseDigits [240920..789857])
