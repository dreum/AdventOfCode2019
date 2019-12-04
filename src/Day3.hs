module Day3 
    ( WirePath (..),
      WireSegment (..),
      Point (..),
      Intersection (..),
      parseWirePath,
      findLineIntersection,
      findClosestIntersection,
      convert,
      runDay3
    ) where

import Data.List.Split

newtype Point = Point (Int, Int) deriving (Show, Eq)
data Intersection = Intersection { getX::Int, getY::Int } deriving (Show, Eq)
newtype WireSegment = WireSegment (Point, Point) deriving (Show, Eq)
type WirePath = [Point]

findClosestIntersectionImpl :: Int -> [Intersection] -> Int
findClosestIntersectionImpl min [] = min
findClosestIntersectionImpl min (i:is) = 
    let currentMin = (abs $ getX i) + (abs $ getY i)
        newMin | min < currentMin = min
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
      (sx1 >= sx2 && sx2 >= ex1)) &&
      not (sx2 == 0 && sy1 == 0)  = Just (Intersection sx2 sy1)
    | otherwise = Nothing

findLineIntersection :: WireSegment -> WireSegment -> Maybe Intersection
findLineIntersection ws1 ws2 
    | isHorizontal ws1 = getIntersection ws1 ws2
    | isHorizontal ws2 = getIntersection ws2 ws1
    | otherwise = Nothing

convert :: WirePath -> [WireSegment]
convert [] = []
convert (x:[]) = []
convert (x:y:xs) = WireSegment (x, y) : convert (y:xs)

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

findWireIntersections :: WirePath -> WirePath -> [Intersection]
findWireIntersections wp1 wp2 = do
                                ws1 <- convert wp1
                                ws2 <- convert wp2
                                maybeToList $ findLineIntersection ws1 ws2 

parsePoint :: String -> Point -> Point
parsePoint (z:zs) (Point (x,y)) = case z of 
    'R' -> Point (x + a, y)
    'U' -> Point (x, y + a)
    'L' -> Point (x - a, y)
    'D' -> Point (x, y - a)
    where a = read zs

parseWirePathImpl :: [String] -> Point -> WirePath
parseWirePathImpl [] p = [p]
parseWirePathImpl (x:xs) p = p : parseWirePathImpl xs newPoint
    where newPoint = parsePoint x p

parseWirePath :: String -> WirePath
parseWirePath s = parseWirePathImpl (splitOn "," s) (Point (0,0))
            
runDay3 :: [String] -> String
runDay3 [wire1, wire2] = "Day3 part 1:\n" ++
                         (show . findClosestIntersection $ findWireIntersections (parseWirePath wire1) (parseWirePath wire2)) ++ "\n"

