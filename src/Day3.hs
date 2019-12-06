module Day3 
    ( WirePath (..),
      WireSegment (..),
      Point (..),
      Intersection (..),
      parseWirePath,
      findLineIntersection,
      findClosestIntersection,
      convert,
      runDay3,
      part1,
      part2
    ) where

import Data.List.Split
import Data.Maybe

newtype Point = Point (Int, Int) deriving (Show, Eq)
data Intersection = Intersection { getX::Int, getY::Int } deriving (Show, Eq)
newtype WireSegment = WireSegment (Point, Point) deriving (Show, Eq)
type WirePath = [(Point, Int)]

findClosestIntersectionImpl :: Int -> [Intersection] -> Int
findClosestIntersectionImpl min [] = min
findClosestIntersectionImpl min (i:is) = 
    let currentMin = (abs $ getX i) + (abs $ getY i)
        newMin | currentMin == 0 = min
               | min == 0 = currentMin
               | min < currentMin = min
               | otherwise = currentMin
    in findClosestIntersectionImpl newMin is

findClosestIntersection :: [Intersection] -> Int
findClosestIntersection [] = 0
findClosestIntersection (i:is) = findClosestIntersectionImpl ((abs $ getX i) + (abs $ getY i)) is 

isHorizontal (WireSegment (Point (sx,sy), Point (ex, ey))) = sy == ey

getIntersection (WireSegment (Point (sx1,sy1), Point (ex1, ey1)))
                (WireSegment (Point (sx2,sy2), Point (ex2, ey2)))
    | ((sy2 <= sy1 && sy1 <= ey2) ||
      (sy2 >= sy1 && sy1 >= ey2)) &&
      ((sx1 <= sx2 && sx2 <= ex1) ||
      (sx1 >= sx2 && sx2 >= ex1)) = Just (Intersection sx2 sy1)
    | otherwise = Nothing

findLineIntersection :: WireSegment -> WireSegment -> Maybe Intersection
findLineIntersection ws1 ws2 
    | isHorizontal ws1 = getIntersection ws1 ws2
    | isHorizontal ws2 = getIntersection ws2 ws1
    | otherwise = Nothing

convert :: WirePath -> [(WireSegment, Int)]
convert [] = []
convert (x:[]) = []
convert ((x,a):(y,b):xs) = (WireSegment (x, y), a) : convert ((y,b):xs)

dist :: Intersection -> WireSegment -> Int
dist i (WireSegment (Point (x,y), _)) = (abs (getX i - x)) + (abs (getY i -y))

intersectionsAndCumulativeSteps :: WirePath -> WirePath -> [(Intersection, Int)]
intersectionsAndCumulativeSteps wp1 wp2 = do
                                (ws1, a) <- convert wp1
                                (ws2, b) <- convert wp2
                                let intersectionAndCumulativeSteps = (\i -> (i, a+b+(dist i ws1)+(dist i ws2)))
                                maybeToList $ fmap intersectionAndCumulativeSteps $ findLineIntersection ws1 ws2 

parsePoint :: String -> (Point, Int) -> (Point, Int)
parsePoint (z:zs) (Point (x,y), oldSteps) = case z of 
    'R' -> (Point (x + a, y), cumulativeSteps)
    'U' -> (Point (x, y + a), cumulativeSteps)
    'L' -> (Point (x - a, y), cumulativeSteps)
    'D' -> (Point (x, y - a), cumulativeSteps)
    where a = read zs
          cumulativeSteps = oldSteps + a

parseWirePathImpl :: [String] -> (Point, Int) -> WirePath
parseWirePathImpl [] p = [p]
parseWirePathImpl (x:xs) p = p : parseWirePathImpl xs newPoint
    where newPoint = parsePoint x p

parseWirePath :: String -> WirePath
parseWirePath s = parseWirePathImpl (splitOn "," s) (Point (0,0), 0)

findWireIntersections w1 w2 = fmap fst $ intersectionsAndCumulativeSteps (parseWirePath w1) (parseWirePath w2)
findSteps w1 w2 = fmap snd $ intersectionsAndCumulativeSteps (parseWirePath w1) (parseWirePath w2)

findMinimum steps = steps !! 1
part1 w1 w2 = show $ findClosestIntersection $ findWireIntersections w1 w2
part2 w1 w2 = show $ findMinimum $ findSteps w1 w2
            
runDay3 :: [String] -> String
runDay3 [wire1, wire2] = "Day3 part 1:\n" ++
                         (part1 wire1 wire2) ++ "\n" ++
                         "part 2: \n" ++
                         (part2 wire1 wire2) ++ "\n"

