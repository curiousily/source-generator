{-# LANGUAGE CPP #-}
module Main (main) where

import Data.List.Split
import Generator
import Options.Applicative

version = "1.0"

data Input = Input {
        includes :: String ,
        name :: String,
        baseName :: String,
        properties :: String,
        interactive :: Bool
        }
        deriving Show

input :: Parser Input
input = Input
    <$> strOption
        ( long "includes"
        <> short 'i'
        <> metavar "LIST"
        <> help "List of comma separated header files to include (Eg. string,vector,boost)" )
    <*> strOption
        ( long "name"
        <> short 'n'
        <> metavar "CLASS NAME"
        <> help "Name of the class" )
    <*> strOption
        ( long "baseName"
        <> short 'b'
        <> metavar "BASE"
        <> help "Name of the base class" )
    <*> strOption
        ( long "properties"
        <> short 'p'
        <> metavar "LIST"
        <> help "List of comma separated properties in the format - type:name (Eg. s:name, i:age)" )
    <*> switch
        ( long "interactive"
        <> short 'e'
        <> help "Enable interactive mode" )

handler :: Input -> IO ()
handler (Input _ _ _ _ True) = interactiveMode
handler (Input is n pn ps _) = do
    let includes = map (\s -> Include s) $ splitComma is
    let properties = map (\(t, n) -> ClassProperty n (propertyType t)) $ parseProperties ps
    let cls = Class includes n pn properties
    wClass cls

parseProperties :: String -> [(String, String)]
parseProperties ps = map (\p -> toTuple $ splitOn ":" p) $ splitComma ps
    where
        toTuple l = (head l, last l)

main :: IO ()
main = execParser opts >>= handler
    where
        opts = info (helper <*> input)
            ( fullDesc
            <> progDesc "Generate C++ class"
            <> header "source-generator - Source code generator" )

interactiveMode :: IO ()
interactiveMode = do
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
    let properties = map (\(t, n) -> ClassProperty n (propertyType t)) propertyInput
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
                        let property = (pType, pName)
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
