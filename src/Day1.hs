module Day1
    ( fuelRequired
    ) where

-- warning, this function doesn't account for low/negative mass numbers
fuelRequired :: Integer -> Integer
fuelRequired mass = mass `div` 3 - 2
