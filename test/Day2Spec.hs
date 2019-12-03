module Day2Spec (spec) where

import Test.Hspec
import Day2

spec :: Spec
spec = do 
    describe "Day2" $ do
        it "can add two positions together" $ do
            intCode 0 [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
        it "can multiply two positions together" $ do
            intCode 0 [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
