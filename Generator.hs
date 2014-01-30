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
	propertyTemplate <- Bs.readFile "templates/cpp-property.st"
	let propertiesString = replaceProperties (cProperties cls) (B.toString propertyTemplate)
	headerTmpl <- headerTemplate
	let replacedProperties = replace "$properties$" propertiesString headerTmpl
	let replaced = replace "$className$" (cName cls) replacedProperties
	let replacedParent = replace "$parentName$" (cParent cls) replaced
	Bs.writeFile outHeaderPath (B.fromString replacedParent)
		where
			className = cName cls
			outHeaderPath = "gen/" ++ className ++ ".h"
			outSourcePath = "gen/" ++ className ++ ".cpp"

headerTemplate = do
	template <- Bs.readFile "templates/cpp-header.st"
	return $ B.toString template

replaceProperties ps t =
	foldl (++) "" (map addProperty ps)
		where
			addProperty p = replaceType p (replaceName p t) 
			replaceType p t = replace "$propertyType$" (typeString(pType p)) t
			replaceName p t = replace "$propertyName$" (pName p) t
