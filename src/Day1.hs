module Day1
    ( fuelRequired
    ) where

fuelRequired :: Integer -> Integer
fuelRequired mass = mass `div` 3 - 2
