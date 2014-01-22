import System.Environment
import System.IO
import Control.Monad

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
	h <- openFile ((cName cls) ++ ".h") WriteMode
	declarationStart (wLine h) cls
	addPublic (wLine h)
	addConstructor (wLine h) cls
	addDestructor (wLine h) cls
	addProperties (wLine h) (cProperties cls)
	declarationEnd (wLine h) cls

wLine h line = do
	hPutStrLn h line

declarationStart w cls =
	w $ "class " ++ cName cls ++ " : public " ++ cParent cls ++ " {"

addPublic w =
	w "public:"

addProperties w ps =
	mapM addProperty ps
		where
			addProperty p = w $ "  " ++ typeString(pType p) ++ " " ++ pName p ++ ";"

addConstructor w cls =
	w $ "  " ++ cName cls ++ "();"

addDestructor w cls = 
	w $ "  virtual ~" ++ cName cls ++ "();"

declarationEnd w cls = w "};"

