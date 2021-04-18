module Entity.Languages.Python
    where

import Entity.Core
import Language.Python.Common hiding (int)
import System.Process
import Entity.Util
import Data.HVect hiding (curry)
import Entity.Entity
import Data.Bifunctor

type PythonAST = Module ()

python :: Language PythonAST
python = Language prettyWritePython
    where
        prettyWritePython path ast = do
            appendFile path $ prettyText ast
            callCommand $ "black -q " ++ path

moduleToPath :: String -> FilePath
moduleToPath = map (\c -> if c == '.' then '/' else c)

fromImport :: [String] -> [String] -> Statement ()
fromImport package items = FromImport (ImportRelative 0 (Just $ dots package) ()) (FromItems (map (\item -> FromItem (Ident item ()) Nothing ()) items) ()) ()

pyClass :: String -> [String] -> Suite () -> Statement ()
pyClass name super body = Class (Ident name ()) (map (\s -> ArgExpr (Var (Ident s ()) ()) ()) super) body ()

assign :: String -> Expr () -> Statement ()
assign name expr = Assign [Var (Ident name ()) ()] expr ()

fun :: String -> [Parameter ()] -> Maybe (Expr ()) -> Suite () -> Statement ()
fun name params anot body = Fun (Ident name ()) params anot body ()

param :: String -> Maybe (Expr ()) -> Maybe (Expr ()) -> Parameter ()
param name anot defValue = Param (Ident name ()) anot defValue ()

selfParam :: Parameter ()
selfParam = param "self" Nothing Nothing

pyReturn :: Expr () -> Statement ()
pyReturn expr = Return (Just expr) ()

string :: String -> Expr ()
string s = Strings ["'", s, "'"] ()

int :: Integer -> Expr ()
int v = Int v (show v) ()

dots :: [String] -> [Ident ()]
dots = map (`Ident` ())

data DjangoField
    = DjangoCharField { maxLength :: Integer }
    | DjangoTextField
    | DjangoIntegerField
    | DjangoDateField

defaultDjangoField :: FieldType -> DjangoField
defaultDjangoField StringField  = DjangoCharField { maxLength = 255 }
defaultDjangoField IntegerField = DjangoIntegerField
defaultDjangoField DateField    = DjangoDateField




data PythonModelInterface e = PythonModelInterface { modelModule :: String, modelName :: String, primaryKey :: String, modelFields :: [(String, FieldType)]}

mkPythonModel :: (Entity e) => e -> String -> PythonModelInterface e
mkPythonModel entity modelMod  = PythonModelInterface modelMod (toPascal $ entityName entity) "id" df
    where
        df = map (first toSnake) $ entityFields entity

fieldToStatement :: DjangoField -> Expr ()
fieldToStatement (DjangoCharField l) = Call (Dot (Var (Ident "models" ()) ()) (Ident "CharField" ()) ()) [ArgKeyword (Ident "max_length" ()) (int l) ()] ()
fieldToStatement DjangoTextField = Call (Dot (Var (Ident "models" ()) ()) (Ident "TextField" ()) ()) [] ()
fieldToStatement DjangoIntegerField = Call (Dot (Var (Ident "models" ()) ()) (Ident "IntegerField" ()) ()) [] ()
fieldToStatement DjangoDateField = Call (Dot (Var (Ident "models" ()) ()) (Ident "DateField" ()) ()) [] ()

djangoModel :: e -> ComponentGenerator PythonAST '[PythonModelInterface e]
djangoModel _ = ComponentGenerator python (\(itf :&: HNil) -> gen itf)
    where
        gen PythonModelInterface {..} = (fp, ast)
            where
                fp = moduleToPath modelModule ++ ".py"
                ast = Module [ fromImport ["django", "db"] ["models"]
                             , pyClass modelName ["models.Model"] (map (\(fn, ft) -> assign fn (fieldToStatement $ defaultDjangoField ft)) $ filter (\(fn, _) -> fn /= "id") modelFields)
                             ]



data PythonRepoInterface e = PythonRepoInterface { repoModule :: String, repoName :: String, listName :: String, getName :: String, saveName :: String, createName :: String}

mkPythonRepo :: (Entity e) => e -> String -> PythonRepoInterface e
mkPythonRepo entity repoMod = PythonRepoInterface repoMod (toPascal $ entityName entity ++ ["repo"]) "list" "get" "save" "create"

djangoRepo :: (Entity e) => e -> ComponentGenerator PythonAST '[PythonRepoInterface e, PythonModelInterface e]
djangoRepo entity = ComponentGenerator python (\(itf :&: modItf :&: HNil) -> gen itf modItf)
    where
        gen PythonRepoInterface {..} PythonModelInterface {..} = (fp, ast)
            where
                fp = moduleToPath repoModule ++ ".py"
                ast = Module $ [fromImport [modelModule] [modelName] | modelModule /= repoModule]
                             ++ [ pyClass repoName []
                                    [ listFun
                                    , getFun
                                    , saveFun
                                    , createFun
                                    ]
                                ]
                eName = toSnake $ entityName entity
                fNames = filter (/= "id") $ map (toSnake . fst) $ entityFields entity
                fParams = map (\n -> param n Nothing Nothing) fNames
                fArgs = map (\n -> ArgKeyword (Ident n ()) (Var (Ident n ()) ()) ()) fNames
                listFun = Decorated [Decorator [Ident "staticmethod" ()] [] ()] (fun listName [selfParam] Nothing [pyReturn $ Call (Var (Ident (modelName ++ ".objects.all") ()) ()) [] ()]) ()
                getFun = Decorated [Decorator [Ident "staticmethod" ()] [] ()] (fun getName [selfParam, param primaryKey Nothing Nothing] Nothing [pyReturn $ Call (Var (Ident (modelName ++ ".objects.get") ()) ()) [ArgExpr (Var (Ident primaryKey ()) ()) ()] ()]) ()
                saveFun = Decorated [Decorator [Ident "staticmethod" ()] [] ()] (fun saveName [selfParam, param eName Nothing Nothing] Nothing [StmtExpr (Call (Var (Ident (eName ++ ".save") ()) ()) [] ()) ()]) ()
                createFun = Decorated [Decorator [Ident "staticmethod" ()] [] ()] (fun createName (selfParam : fParams) Nothing [pyReturn $ Call (Var (Ident (modelName ++ ".objects.create") ()) ()) fArgs ()]) ()



data JSONField
    = JSONStringField
    | JSONNumberField

defaultJSONField :: FieldType -> JSONField
defaultJSONField StringField  = JSONStringField
defaultJSONField IntegerField = JSONNumberField
defaultJSONField DateField    = JSONStringField

data RESTInterface e = RESTInterface { endpoint :: String, jsonFields :: [(String, JSONField)]}

mkREST :: (Entity e) => e -> String -> RESTInterface e
mkREST entity url = RESTInterface url df
    where
        df = map (bimap toSnake defaultJSONField) $ entityFields entity

newtype PythonRESTInterface e = PythonRESTInterface { restModule :: String }

mkPythonREST :: (Entity e) => e -> String -> PythonRESTInterface e
mkPythonREST _ = PythonRESTInterface

toDict :: String -> PythonModelInterface e -> RESTInterface e -> Expr ()
toDict paramName pItf rItf = Dictionary (zipWith (curry (\((pName, pType), (rName, rType)) -> DictMappingPair (string rName) (convert pName pType rType) )) (modelFields pItf) (jsonFields rItf)) ()
    where
        convert fieldName pt rt = Var (Ident (paramName ++ "." ++ fieldName) ()) ()

fromDict :: String -> PythonModelInterface e -> RESTInterface e -> Expr ()
fromDict paramName pItf rItf = Call (Var (Ident (modelName pItf) ()) ()) (zipWith (curry (\((pName, pType), (rName, rType)) -> ArgKeyword (Ident pName ()) (convert rName rType pType) ())) (modelFields pItf) (jsonFields rItf)) ()
    where
        convert fieldName rt pt = Var (Ident (paramName ++ "['" ++ fieldName ++ "']") ()) ()

djangoRESTView :: (Entity e) => e -> ComponentGenerator PythonAST '[PythonRESTInterface e, RESTInterface e, PythonRepoInterface e, PythonModelInterface e]
djangoRESTView entity = ComponentGenerator python (\(itf :&: restItf :&: repoItf :&: modelItf :&: HNil) -> gen itf restItf repoItf modelItf)
    where
        gen PythonRESTInterface {..} rItf@RESTInterface {..} PythonRepoInterface {..} pItf@PythonModelInterface {..} = (fp, ast)
            where
                fp = moduleToPath restModule ++ ".py"
                listViewName = toPascal $ "list" : entityName entity ++ ["view"]
                ast = Module $ [ fromImport [repoModule] [repoName] | restModule /= repoModule ]
                            ++ [ fromImport ["django", "http"] ["JsonResponse", "Http404"]
                               , fromImport ["django", "utils", "decorators"] ["method_decorator"]
                               , fromImport ["django", "views"] ["View"]
                               , fromImport ["django", "views", "decorators", "csrf"] ["csrf_exempt"]
                               , fromImport ["django", "urls"] ["path"]
                               , Import [ImportItem [Ident "json" ()] Nothing ()] ()
                               , pyClass listViewName []
                                    [ getListFun
                                    , postFun
                                    , dispatchFun
                                    ]
                               , registerFun
                               ]
                eName = toSnake $ entityName entity
                listDict = ListComp (Comprehension (ComprehensionExpr (toDict eName pItf rItf)) (CompFor False [Var (Ident eName ()) ()] (Call (Var (Ident (repoName ++ "." ++ listName) ()) ()) [] ()) Nothing ()) ()) ()
                getListFun = fun "get" [selfParam, param "request" Nothing Nothing] Nothing [pyReturn $ Call (Var (Ident "JsonResponse" ()) ()) [ArgExpr listDict (), ArgKeyword (Ident "safe" ()) (Bool False ()) ()] ()]
                postFun = fun "post" [selfParam, param "request" Nothing Nothing] Nothing
                    [ assign "body" $ Call (Var (Ident "json.loads" ()) ()) [ArgExpr (Var (Ident "request.body" ()) ()) ()] ()
                    , StmtExpr (Call (Var (Ident (repoName ++ "." ++ saveName) ()) ()) [ArgExpr (fromDict "body" pItf rItf) ()] ()) ()
                    , pyReturn $ Call (Var (Ident "JsonResponse" ()) ()) [ArgExpr (Dictionary [DictMappingPair (string "message") (string "success")] ()) (), ArgKeyword (Ident "status" ()) (int 204) ()] ()
                    ]
                dispatchFun = Decorated [Decorator [Ident "method_decorator" ()] [ArgExpr (Var (Ident "csrf_exempt" ()) ()) ()] ()] (fun "dispatch" [selfParam, VarArgsPos (Ident "args" ()) Nothing (), VarArgsKeyword (Ident "kwargs" ()) Nothing ()] Nothing [pyReturn $ Call (Var (Ident "super.dispatch" ()) ()) [ArgVarArgsPos (Var (Ident "args" ()) ()) (), ArgVarArgsKeyword (Var (Ident "kwargs" ()) ()) ()] ()]) ()
                registerFun = fun "register_urls" [param "urls" Nothing Nothing] Nothing [AugmentedAssign (Var (Ident "urls" ()) ()) (PlusAssign ()) (List [Call (Var (Ident "path" ()) ()) [ArgExpr (string endpoint) (), ArgExpr (Call (Var (Ident (listViewName ++ "as_view") ()) ()) [] ()) ()] ()] ()) ()] 
