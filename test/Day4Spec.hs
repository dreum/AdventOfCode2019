module Day4Spec (spec) where

import Test.Hspec
import Day4

spec = do 
    describe "Duplicates" $ do
        it "reports true for duplicates" $ do
            hasDuplicate [1,1] `shouldBe` True
    describe "Duplicates" $ do
        it "reports false for non-duplicates" $ do
            hasDuplicate [1,2] `shouldBe` False
    describe "Non decreasing" $ do
        it "reports true for increasing numbers" $ do
            nonDecreasing [1,2] `shouldBe` True
    describe "Non decreasing" $ do
        it "reports false for decreasing numbers" $ do
            nonDecreasing [2,1] `shouldBe` False
    describe "parsing" $ do
        it "can parse correctly" $ do
            (parseDigits 523456) `shouldBe` [5,2,3,4,5,6] 
