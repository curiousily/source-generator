import System.Environment
import System.IO
import Control.Monad
import Data.String.Utils
import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as B

data PropertyType = String | Int | Bool | Float | StringList | IntList

typeString :: PropertyType -> String
typeString String = "std::string"
typeString Int = "int"
typeString Bool = "bool"
typeString Float = "float"
typeString StringList = "std::vector<std::string>"
typeString IntList = "std::vector<int>"

data Property = Property { 
			pName :: String,
			pType :: PropertyType
			}

data Class = Class {
		cName :: String,
		cParent :: String,
		cProperties :: [Property]
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
	propertiesContent <- propertiesContent cls
	headerTmpl <- headerTemplate
	let content = replaceAll ["$properties$", "$className$", "$parentName$"] [propertiesContent, cName cls, cParent cls] headerTmpl 
	return $ B.fromString content

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
replaceAll (x:xs) (y:ys) t = replaceAll xs ys t'
	where
		t' = replace x y t

replaceProperties ps t =
	foldl (++) "" (map addProperty ps)
		where
			addProperty p = replaceType p (replaceName p t) 
			replaceType p t = replace "$propertyType$" (typeString(pType p)) t
			replaceName p t = replace "$propertyName$" (pName p) t
