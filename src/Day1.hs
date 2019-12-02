module Day1
    ( fuelRequired,
      totalFuelRequired,
      runDay1
    ) where

-- warning, this function doesn't account for low/negative mass numbers
fuelRequired :: Integer -> Integer
fuelRequired mass = mass `div` 3 - 2

totalFuelRequired :: Integer -> Integer
totalFuelRequired mass 
        | fuelMass > 0 = fuelMass + totalFuelRequired fuelMass
        | otherwise = 0
    where fuelMass = fuelRequired mass 

runDay1 :: [Integer] -> String
runDay1 masses =  
    "Day 1, part 1 results:\n" ++ 
    (show $ sum $ fmap fuelRequired masses) ++ "\n" ++
    "Day 1, part 2 results:\n" ++
    (show $ sum $ fmap totalFuelRequired masses)

