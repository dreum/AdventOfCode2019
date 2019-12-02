module Day1
    ( fuelRequired,
      totalFuelRequired
    ) where

-- warning, this function doesn't account for low/negative mass numbers
fuelRequired :: Integer -> Integer
fuelRequired mass = mass `div` 3 - 2

totalFuelRequired :: Integer -> Integer
totalFuelRequired mass 
        | fuelMass > 0 = fuelMass + totalFuelRequired fuelMass
        | otherwise = 0
    where fuelMass = fuelRequired mass 

