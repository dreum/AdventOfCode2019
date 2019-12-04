module Day3Spec (spec) where

import Test.Hspec
import Day3

unwrapPoint :: Point -> (Int, Int)
unwrapPoint (Point w) = w

unwrapWirePath :: WirePath -> [(Int, Int)]
unwrapWirePath = fmap unwrapPoint 

spec :: Spec
spec = do 
    describe "Day3" $ do
        it "can parse input" $ do
            (unwrapWirePath $ parseWirePath "L5,U4,R6,D3") `shouldBe` [(0,0),(-5,0),(-5,4),(1,4),(1,1)]
