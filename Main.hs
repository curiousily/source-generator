module Main (main) where

import Data.List.Split
import Generator

version = "1.0"

main :: IO ()
main = do
	putStrLn $ "Source Generator v" ++ version
	putStrLn "Class name"
	className <- getLine
	putStrLn "Include files"
	includeFiles <- getLine
	let includes = map (\s -> Include s) $ splitComma includeFiles
	putStrLn "Includes"

splitComma :: String -> [String]
splitComma = splitOn ","
