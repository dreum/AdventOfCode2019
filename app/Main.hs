module Main where

import Day1
import Day2
import System.IO
import System.Exit
import System.Environment
import Data.Char

switchOnDay :: String -> String -> String
switchOnDay day contents
    | (fmap toLower day) == "day1" = runDay1 $ fmap read $ lines contents
    | (fmap toLower day) == "day2" = runDay2 $ read contents
    | otherwise = "invalid input: please include a valid day you want to run"

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [dayArgument] -> do   
            contents <- getContents
            let output = switchOnDay dayArgument contents
            putStrLn output
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " dayN"
            exitFailure

