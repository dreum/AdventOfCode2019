module Day1Spec (spec) where

import Test.Hspec
import Day1

spec :: Spec
spec = do 
    describe "Day1" $ do
        it "given mass, fuel required is mass / 3 - 2" $ do
            fuelRequired 12 `shouldBe` (2 :: Int)
        it "fuelRequired handles rounding integers down" $ do
            fuelRequired 14 `shouldBe` (2 :: Int)
        it "fuelRequired extra test 1" $ do
            fuelRequired 1969 `shouldBe` (654 :: Int)
        it "fuelRequired extra test 2" $ do
            fuelRequired 100756 `shouldBe` (33583 :: Int)
        it "given that the fuel mass is small, no additional fuel is required for the total" $ do
            totalFuelRequired 14 `shouldBe` (2 :: Int)
        it "totalFuelRequired takes fuel mass into account" $ do
            totalFuelRequired 1969 `shouldBe` (966 :: Int)
        it "totalFuelRequired extra test 1" $ do
            totalFuelRequired 100756 `shouldBe` (50346 :: Int)
