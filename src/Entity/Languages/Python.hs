module Entity.Languages.Python 
    ( python
    , DjangoField (..)
    , djangoModel
    ) where

import Entity.Core
import Language.Python.Common hiding (int)
import System.Process
import Entity.Util

type PythonAST = Module ()

python :: Language PythonAST
python = Language prettyWritePython
    where
        prettyWritePython path ast = do
            writeFile path $ prettyText ast
            callCommand $ "black -q " ++ path

fromImport :: [String] -> [String] -> Statement ()
fromImport package items = FromImport (ImportRelative 0 (Just $ dots package) ()) (FromItems (map (\item -> FromItem (Ident item ()) Nothing ()) items) ()) ()

pyClass :: String -> [String] -> Suite () -> Statement ()
pyClass name super body = Class (Ident name ()) (map (\s -> ArgExpr (Var (Ident s ()) ()) ()) super) body ()

assign :: String -> Expr () -> Statement ()
assign name expr = Assign [Var (Ident name ()) ()] expr ()

int :: Integer -> Expr ()
int v = Int v (show v) ()

dots :: [String] -> [Ident ()]
dots = map (`Ident` ())

data DjangoField
    = DjangoCharField { maxLength :: Integer }
    | DjangoTextField
    | DjangoIntegerField
    | DjangoDateField

fieldToStatement :: DjangoField -> Expr ()
fieldToStatement (DjangoCharField l) = Call (Dot (Var (Ident "models" ()) ()) (Ident "CharField" ()) ()) [ArgKeyword (Ident "max_length" ()) (int l) ()] ()
fieldToStatement DjangoTextField = Call (Dot (Var (Ident "models" ()) ()) (Ident "TextField" ()) ()) [] ()
fieldToStatement DjangoIntegerField = Call (Dot (Var (Ident "models" ()) ()) (Ident "IntegerField" ()) ()) [] ()
fieldToStatement DjangoDateField = Call (Dot (Var (Ident "models" ()) ()) (Ident "DateField" ()) ()) [] ()

djangoModel :: EntityModel PythonAST DjangoField
djangoModel = EntityModel python df re (Just $ const "entities.py")
    where
        df StringField  = DjangoCharField { maxLength = 255 }
        df IntegerField = DjangoIntegerField
        df DateField    = DjangoDateField

        re (Entity name fields) = 
            Module [ fromImport ["django", "db"] ["models"]
                   , pyClass (toPascal name) ["models.Model"] (map (\(fn, ft) -> assign (toSnake fn) (fieldToStatement ft)) fields)
                   ]

