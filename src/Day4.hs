module Day4 
    ( hasDuplicate,
      hasDuplicate2,
      nonDecreasing,
      parseDigits,
      runDay4
    ) where

import Control.Monad
import Data.List

listToPairs :: [a] -> [(a,a)]
listToPairs [] = []
listToPairs (x:[]) = []
listToPairs (x:y:xs) = (x,y) : listToPairs (y:xs)

hasDuplicate :: [Int] -> Bool
hasDuplicate xs = any (uncurry (==)) $ listToPairs xs

hasDuplicate2 :: [Int] -> Bool
hasDuplicate2 xs = any (\x -> length x == 2) $ group xs

nonDecreasing :: [Int] -> Bool
nonDecreasing xs = all (uncurry (<=)) $ listToPairs xs

charToString c = [c]

parseDigits :: Int -> [Int]
parseDigits s = map (read . charToString) $ show s

runDay4  = "Day4 part 1:\n" ++
           (show $ length $ filter filterFunction $ map parseDigits [240920..789857]) ++ "\n" ++
           "part 2:\n" ++
           (show $ length $ filter filterFunction2 $ map parseDigits [240920..789857])
            where filterFunction = liftM2 (&&) hasDuplicate nonDecreasing
                  filterFunction2 = liftM2 (&&) hasDuplicate2 nonDecreasing
