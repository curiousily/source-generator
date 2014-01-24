import System.Environment
import System.IO
import Control.Monad
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as B

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
	let headerContent = B.pack . header $ cls
	Bs.writeFile headerFileName headerContent
	let sourceContent = B.pack . source $ cls
	Bs.writeFile sourceFileName sourceContent
		where
			className = cName cls
			headerFileName = className ++ ".h"
			sourceFileName = className ++ ".cpp"

include h = "#include " ++ "\"" ++ h ++ ".h\""

source cls = do
	include (cName cls) ++ "\n\n" ++ cName cls ++ "::" ++ cName cls ++ "(){}\n" ++ cName cls ++ "::~" ++ cName cls ++ "(){}\n"

header cls = 
	declarationStart cls ++ public ++ properties (cProperties cls) ++ constructor cls ++ destructor cls ++ declarationEnd cls

declarationStart cls =
	"class " ++ cName cls ++ " : public " ++ cParent cls ++ " {\n"

public =
	"public:\n"

properties ps =
	foldl (++) "" (map addProperty ps)
		where
			addProperty p = "  " ++ typeString(pType p) ++ " " ++ pName p ++ ";\n"

constructor cls = "  " ++ cName cls ++ "();\n"

destructor cls = "  virtual ~" ++ cName cls ++ "();\n"

declarationEnd cls = "};\n"

