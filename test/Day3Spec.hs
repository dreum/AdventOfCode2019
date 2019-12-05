module Day3Spec (spec) where

import Test.Hspec
import Day3

unwrapPoint :: Point -> (Int, Int)
unwrapPoint (Point w) = w

unwrapWirePath :: WirePath -> [(Int, Int)]
unwrapWirePath = fmap (unwrapPoint . fst)

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
            convert [(Point (0,0),0), (Point (1,0),1)] `shouldBe` [(WireSegment (Point (0,0), Point (1,0)),0)]

    describe "full algo" $ do
        it "part1 works" $ do
            part1 "R8,U5,L5,D3" "U7,R6,D4,L4"
            `shouldBe` "6"
        it "part2 works" $ do
            part2 "R8,U5,L5,D3" "U7,R6,D4,L4"
            `shouldBe` "30"
        it "part2 works 2" $ do
            part2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
            `shouldBe` "610"
        it "part2 works 3" $ do
            part2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
            `shouldBe` "410"
