module Day2 (intCode) where

import Control.Lens

opcodeLookup :: Int -> Int -> Int -> Int
opcodeLookup opcode
    | opcode == 1 = (+)
    | opcode == 2 = (*)

intCode :: Int -> [Int] -> [Int]
intCode startPos codes = let 
            operator = opcodeLookup $ codes !! startPos
            arg1 = codes !! (codes !! (startPos + 1))
            arg2 = codes !! (codes !! (startPos + 2))
            resultPos = codes !! (startPos + 3)
            result = operator arg1 arg2
            in (element resultPos .~ result) codes

