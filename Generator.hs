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
	template <- Bs.readFile "templates/cpp-header.st"
	let templateContent = B.toString template
	
	propertyTemplate <- Bs.readFile "templates/cpp-property.st"
	let replaced = replace "$className$" (cName cls) templateContent
	let replacedParent = replace "$parentName$" (cParent cls) replaced
	Bs.writeFile headerFilePath (B.fromString replacedParent)
		where
			className = cName cls
			headerFilePath = "gen/" ++ className ++ ".h"
			sourceFilePath = "gen/" ++ className ++ ".cpp"

properties ps =
	foldl (++) "" (map addProperty ps)
		where
			addProperty p = "  " ++ typeString(pType p) ++ " " ++ pName p ++ ";\n"
