module Day1
    ( fuelRequired,
      totalFuelRequired,
      runDay1
    ) where

-- warning, this function doesn't account for low/negative mass numbers
fuelRequired :: Int -> Int
fuelRequired mass = mass `div` 3 - 2

totalFuelRequired :: Int -> Int
totalFuelRequired mass = sum $ takeWhile (> 0) $ iterate fuelRequired (fuelRequired mass)

runDay1 :: [Int] -> String
runDay1 masses =  
    "Day 1, part 1 results:\n" ++ 
    (show $ sum $ fmap fuelRequired masses) ++ "\n" ++
    "Day 1, part 2 results:\n" ++
    (show $ sum $ fmap totalFuelRequired masses)

