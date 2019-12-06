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

hasDuplicate :: (a,b) -> Bool
hasDuplicate = any (uncurry (==)) 

nonDecreasing :: (a,b) -> Bool
nonDecreasing = all (uncurry (<=))

charToString c = [c]

parseDigits :: Int -> [Int]
parseDigits s = map (read . charToString) $ show s

runDay4  = "Day4 part 1:\n" ++
           (show $ length $ filter filterFunction $ map (listToPairs . parseDigits) [240920..789857])
            where filterFunction = liftM2 (&&) hasDuplicate nonDecreasing
