module Day3 
    ( WirePath (..),
      WireSegment (..),
      Point (..),
      Intersection (..),
      parseWirePath
    ) where

import Data.List.Split

newtype Point = Point (Int, Int)
newtype Intersection = Intersection (Int, Int)
newtype WireSegment = WireSegment (Point, Point)
type WirePath = [Point]

-- findClosestIntersection :: [Intersection] -> Intersection

-- findLineIntersection :: WireSegment -> WireSegment -> Intersection

-- findWireIntersection :: WirePath -> WirePath -> [Intersection]

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
            
