module Day2 
    ( intCode,
      intCodeMulti,
      runDay2
    ) where

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

intCodeMultiHelper :: Int -> [Int] -> [Int]
intCodeMultiHelper position codes = let opcode = codes !! position
    in case opcode of 
        99 -> codes
        _ -> intCodeMultiHelper (position + 4) newCodes
            where newCodes = intCode position codes

intCodeMulti :: [Int] -> [Int]
intCodeMulti codes = intCodeMultiHelper 0 codes

runDay2 codes = "Day2 part 1:\n" ++ 
                (show ((intCodeMulti editedCodes1) !! 0))
                where editedCodesTemp = (element 1 .~ 12) codes
                      editedCodes1 = (element 2 .~ 2) editedCodesTemp
