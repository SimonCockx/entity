{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Entity.Entity where

import qualified Language.Haskell.TH as TH
import Entity.Util
import Data.Bifunctor

data FieldType
    = StringField
    | IntegerField
    | DateField
    deriving (Eq, Ord, Show)


class Entity a where
    entityName :: a -> Name
    entityFields :: a -> [(Name, FieldType)]



mkEntity :: String -> [(String, FieldType)] -> TH.Q [TH.Dec]
mkEntity rawName rawFields = do
    let dt = TH.DataD [] dtName [] Nothing [TH.NormalC dtName []] []
    inst <- genDerivingEntity dtName name fields
    return [dt, inst]
    where
        name = fromKebab rawName
        fields = map (first fromKebab) rawFields
        dtName = TH.mkName $ toPascal name

genDerivingEntity :: TH.Name -> Name -> [(Name, FieldType)] -> TH.Q TH.Dec
genDerivingEntity tyName name fields = do
        te <- [t|Entity $(return $ TH.ConT tyName)|]
        return $ TH.InstanceD Nothing [] te decs
    where
        decs =
            [ TH.FunD 'entityName 
                [ TH.Clause [TH.WildP] (TH.NormalB $ nameToExp name) []
                ]
            , TH.FunD 'entityFields 
                [ TH.Clause [TH.WildP] (TH.NormalB $ TH.ListE $ map (\(n, ft) -> TH.TupE [Just $ nameToExp n, Just $ TH.ConE $ TH.mkName $ show ft]) fields) []
                ]
            ]
        nameToExp = TH.ListE . map (TH.LitE . TH.StringL)
