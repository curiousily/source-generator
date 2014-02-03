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
    putStrLn "Parent name"
    parentName <- getLine
    putStrLn "Properties"
    propertyInput <- readProperties
    let properties = map (\(n, t) -> ClassProperty n (propertyType t)) propertyInput
    let cls = Class includes className parentName properties
    wClass cls
    putStrLn $ "Class " ++ className ++ " written to gen dir"

readProperties :: IO [(String, String)]
readProperties = do
    putStrLn "Add property? (y,n)"
    answer <- getLine
    case answer of "n" -> return []
                   "y" -> do
                        putStrLn "Property type (s, i, b, f, sl, il)"
                        pType <- getLine
                        putStrLn "Property name"
                        pName <- getLine
                        let property = (pName, pType)
                        nextProperties <- readProperties
                        return $ property : nextProperties

propertyType :: String -> PropertyType
propertyType "s" = String
propertyType "i" = Int
propertyType "b" = Bool
propertyType "f" = Float
propertyType "sl" = StringList
propertyType "il" = IntList

splitComma :: String -> [String]
splitComma = splitOn ","
