module Generator (Class(..), Include(..), ClassProperty(..), PropertyType(..), wClass) where

import System.Environment
import System.IO
import Control.Monad
import Data.String.Utils
import Data.List
import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as B
import Test.QuickCheck

data PropertyType = String | Int | Bool | Float | StringList | IntList

typeString :: PropertyType -> String
typeString String = "std::string"
typeString Int = "int"
typeString Bool = "bool"
typeString Float = "float"
typeString StringList = "std::vector<std::string>"
typeString IntList = "std::vector<int>"

data ClassProperty = ClassProperty { 
			pName :: String,
			pType :: PropertyType
			}

data Include = Include {
			iFileName :: String
			}

data Class = Class {
		cIncludes :: [Include],
		cName :: String,
		cParent :: String,
		cProperties :: [ClassProperty]
		}

wClass cls = do
	wClassFile outHeaderPath headerContent cls
	wClassFile outSourcePath sourceContent cls
		where
			className = cName cls
			outHeaderPath = "gen/" ++ className ++ ".h"
			outSourcePath = "gen/" ++ className ++ ".cpp"

wClassFile path contentF cls = do
	content <- contentF cls	
	Bs.writeFile path content

headerContent cls = do
	includesContent <- includesContent cls
	propertiesContent <- propertiesContent cls
	headerTmpl <- headerTemplate
	let content = replaceAll ["$includes$", "$properties$", "$className$", "$parentName$"] [includesContent, propertiesContent, cName cls, cParent cls] headerTmpl 
	return $ B.fromString content

includesContent cls = do
	includeTemplate <- Bs.readFile "templates/cpp-include.st"
	return $ replaceIncludes (cIncludes cls) (B.toString includeTemplate)

propertiesContent cls = do
	propertyTemplate <- Bs.readFile "templates/cpp-property.st"
	return $ replaceProperties (cProperties cls) (B.toString propertyTemplate)

sourceContent cls = do
	sourceTmpl <- sourceTemplate
	let content = replace "$className$" (cName cls) sourceTmpl
	return $ B.fromString content
	

headerTemplate = do
	template <- Bs.readFile "templates/cpp-header.st"
	return $ B.toString template

sourceTemplate = do
	template <- Bs.readFile "templates/cpp-source.st"
	return $ B.toString template

replaceAll [] _ t = t
replaceAll _ [] t = t
replaceAll (x:xs) (y:ys) t = replaceAll xs ys t'
	where
		t' = replace x y t

replaceIncludes is t =
	intercalateSpace is addInclude
		where
			addInclude i = replace "$fileName$" (iFileName i) t

replaceProperties ps t =
	intercalateSpace ps addProperty
		where
			addProperty p = replaceType p (replaceName p t) 
			replaceType p t = replace "$propertyType$" (typeString(pType p)) t
			replaceName p t = replace "$propertyName$" (pName p) t

intercalateSpace xs f =
	intercalate "" (map f xs)
	
propSameLength :: [String] -> Bool
propSameLength xs = length (replaceAll xs xs t) == length t
	where
		t = intercalate "" xs
