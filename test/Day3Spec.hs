module Day3Spec (spec) where

import Test.Hspec
import Day3

unwrapPoint :: Point -> (Int, Int)
unwrapPoint (Point w) = w

unwrapWirePath :: WirePath -> [(Int, Int)]
unwrapWirePath = fmap unwrapPoint 

spec :: Spec
spec = do 
    describe "parseWirePath" $ do
        it "can parse input" $ do
            (unwrapWirePath $ parseWirePath "L5,U4,R6,D3") `shouldBe` [(0,0),(-5,0),(-5,4),(1,4),(1,1)]
    describe "intersection" $ do
        it "can find an intersection 1" $ do
            findLineIntersection (WireSegment (Point (0,0), Point (0,4))) 
                                 (WireSegment (Point (-2,2), Point (2,2))) 
            `shouldBe` (Just (Intersection 0 2))
        it "can find an intersection 2" $ do
            findLineIntersection (WireSegment (Point (-2,2), Point (2,2)))
                                 (WireSegment (Point (0,0), Point (0,4))) 
            `shouldBe` (Just (Intersection 0 2))
        it "finds nothing when theres no intersection 1" $ do
            findLineIntersection (WireSegment (Point (0,0), Point (0,4))) 
                                 (WireSegment (Point (-2,5), Point (2,5))) 
            `shouldBe` Nothing
        it "finds nothing when theres no intersection 2" $ do
            findLineIntersection (WireSegment (Point (0,0), Point (0,4))) 
                                 (WireSegment (Point (-2,-5), Point (2,-5))) 
            `shouldBe` Nothing
        it "finds nothing when theres no intersection 3" $ do
            findLineIntersection (WireSegment (Point (0,0), Point (0,4))) 
                                 (WireSegment (Point (1,2), Point (5,2))) 
            `shouldBe` Nothing
        it "finds nothing when theres no intersection 4" $ do
            findLineIntersection (WireSegment (Point (0,0), Point (0,4))) 
                                 (WireSegment (Point (-1,2), Point (-5,2))) 
            `shouldBe` Nothing
    describe "findClosestIntersection" $ do
        it "finds the closest intersection" $ do
            findClosestIntersection [(Intersection 0 2), (Intersection 0 4)] `shouldBe` 2

    describe "WirePath to WireSegments" $ do
        it "works" $ do
            convert [Point (0,0), Point (1,0)] `shouldBe` [WireSegment (Point (0,0), Point (1,0))]

    describe "full algo" $ do
        it "works" $ do
            runDay3 ["R8,U5,L5,D3","U7,R6,D4,L4"]
            `shouldBe` "Day3 part 1:\n6\n"
