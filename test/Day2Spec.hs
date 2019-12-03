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
        it "can return to positions passed the stop code" $ do
            intCode 0 [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
        it "can handle multiple opcodes" $ do
            intCodeMulti [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
