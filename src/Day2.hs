module Day2 
    ( intCode,
      intCodeMulti,
      runDay2
    ) where

import Control.Lens
import Control.Monad

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

editCodes arg1 arg2 codes = (element 1 .~ arg1) tempCodes
                        where tempCodes = (element 2 .~ arg2) codes

part2Runner codes = do 
                arg1 <- [0..99]
                arg2 <- [0..99]
                let editedCodes = editCodes arg1 arg2 codes
                let newCodes = intCodeMulti editedCodes
                guard ((newCodes !! 0) == 19690720) 
                [(arg1, arg2)]

runDay2 codes = "Day2 part 1:\n" ++ 
                (show ((intCodeMulti $ editCodes 12 2 codes) !! 0)) ++ "\n" ++
                "Day2 part 2:\n" ++
                (show $ part2Runner $ codes)
