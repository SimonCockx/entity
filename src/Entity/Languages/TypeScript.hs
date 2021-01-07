module Entity.Languages.TypeScript
    ( typeScript
    , TSField (..)
    , typeScriptModel
    ) where

import Entity.Core
import System.Process
import Language.TypeScript
import Entity.Util

type TypeScriptAST = [DeclarationElement]

typeScript :: Language TypeScriptAST
typeScript = Language prettyWriteTypeScript
    where
        prettyWriteTypeScript path ast = do
            writeFile path $ renderDeclarationSourceFile ast
            callCommand $ "prettier --write --loglevel error " ++ path

export :: Statement -> DeclarationElement
export = StatementDeclaration (Just Exported)

param :: String -> Type -> Parameter
param name t = RequiredOrOptionalParameter Nothing name Nothing (Just t)

tsClass :: Name -> [(Name, TSField)] -> Statement
tsClass name fs = ClassDeclaration (toPascal name) Nothing Nothing Nothing (
    [ FieldDefinition (Just Private) Nothing [] (LeftHand "_isSynced" (Just $ Predefined BooleanType)) Nothing
    , GetterDefinition (Just Public) Nothing "isSynced" (Just $ Predefined BooleanType) (Block [SideEffect $ Return $ Var "this" `Dot` "_isSynced"])
    , FieldDefinition (Just Public) Nothing [Readonly] (LeftHand "id" (Just $ Predefined NumberType)) Nothing
    ]
    ++ concatMap (uncurry fieldToStatement) fs
    ++ [ ConstructorDefinition (Just Public) (param "id" (Predefined NumberType) : map (uncurry fieldToParameter) fs) (Block 
        ( map (setPrivVar . fst) fs
        ++ [ SideEffect $ Assign ["this", "id"] (Var "id")
           , SideEffect $ Assign ["this", "_isSynced"] (BoolLit False)
           ])) ]
    )
    where
        setPrivVar n = SideEffect $ Assign ["this", '_' : toCamel n] (Var $ toCamel n)

data TSField
    = TSStringField
    | TSNumberField
    | TSDateField

fieldToTSType :: TSField -> Type
fieldToTSType TSStringField = Predefined StringType
fieldToTSType TSNumberField = Predefined NumberType
fieldToTSType TSDateField = TypeReference $ TypeRef (TypeName Nothing "Date") Nothing

fieldToStatement :: Name -> TSField -> [ClassElement]
fieldToStatement name field
    = [ FieldDefinition (Just Private) Nothing [] (LeftHand pCamelName (Just ft)) Nothing
      , GetterDefinition (Just Public) Nothing camelName (Just ft) (Block 
            [SideEffect $ Return $ Var "this" `Dot` pCamelName])
      , SetterDefinition (Just Public) Nothing camelName (param newName ft) (Block
            [SideEffect $ Assign ["this", "_isSynced"] (BoolLit False), SideEffect $ Assign ["this", pCamelName] (Var newName)])
      ]
    where
        camelName = toCamel name
        pCamelName = '_' : camelName
        newName = "new" ++ toPascal name
        ft = fieldToTSType field

fieldToParameter :: Name -> TSField -> Parameter
fieldToParameter name field = param (toCamel name) (fieldToTSType field)

typeScriptModel :: EntityModel TypeScriptAST TSField
typeScriptModel = EntityModel typeScript df re (Just $ \n -> toKebab n ++ ".ts")
    where
        df StringField  = TSStringField
        df IntegerField = TSNumberField
        df DateField    = TSDateField

        re Entity {..} = 
            [ export $ tsClass name fields
            ]

