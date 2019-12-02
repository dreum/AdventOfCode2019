module Main where

import Day1
import System.IO
import System.Exit
import System.Environment
import Data.Char

switchOnDay :: String -> [Integer] -> String
switchOnDay day
    | (fmap toLower day) == "day1" = runDay1
    | otherwise = (\_ -> "invalid input: please include a valid day you want to run")

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [dayArgument] -> do   
            contents <- getContents
            let allLines = fmap read $ lines contents
            let runner = switchOnDay dayArgument
            putStrLn $ runner allLines
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " dayN"
            exitFailure

