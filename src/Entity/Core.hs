{-# LANGUAGE TypeSynonymInstances #-}
module Entity.Core where

import Data.Maybe (fromMaybe)
import Control.Monad.Except
import Entity.Util
import Entity.ComponentConfig
import Data.HVect (HVect)


    

data FieldType
    = StringField
    | IntegerField
    | DateField
    deriving (Eq, Ord)


data EntityInfo a = Entity { name :: Name 
                           , fields :: [(Name, a)]
                           }

emap :: (Name -> a -> b) -> EntityInfo a -> EntityInfo b
emap f Entity {..} = Entity name (map (\(n, v) -> (n, f n v)) fields)

instance Functor EntityInfo where
    fmap = emap . const

type Entity = EntityInfo FieldType


newtype Language ast = Language { prettyWrite :: FilePath -> ast -> IO () }



newtype ComponentGenerator requires = ComponentGenerator (HVect requires -> IO ())

data Providers interfaces iss nss where
    PNil :: Providers interfaces '[] '[]
    (:%:) :: (SublistContains ns is interfaces) => ComponentGenerator is -> Providers interfaces iss nss -> Providers interfaces (is ': iss) (ns ': nss)

infixr 5 :%:

generateStructure :: HVect interfaces -> Providers interfaces iss nss -> IO ()
generateStructure _ PNil = return ()
generateStructure interfaces ((ComponentGenerator generate) :%: gens) = do
    generate (findFirsts interfaces)
    generateStructure interfaces gens


data Component ast m
    = Component { componentName :: String
                , language :: Language ast
                , renderComponent :: m ast
                , defaultFilename :: Maybe FilePath
                }

type EntityModel ast m = Entity -> Component ast m

entityModel :: String -> Language ast -> (FieldType -> field) -> (EntityInfo field -> m ast) -> Maybe (Name -> FilePath) -> EntityModel ast m
entityModel modelName lang defaultField renderEntity defFilename entity = Component componentName lang render dname
    where
        componentName = modelName ++ ":" ++ toKebab (name entity)
        render = renderEntity $ fmap defaultField entity
        dname = fmap ($ name entity) defFilename



defaultPath :: FilePath
defaultPath = "./"


writeComponent :: (MonadConfigEnvironment m) => Component ast m -> m (IO ())
writeComponent Component {..} = do
    fn <- catchError (getString componentName "filename") (const defFilename)
    let fp = fromMaybe defaultPath Nothing
        filepath = fp ++ fn
    componentAST <- renderComponent
    return $ prettyWrite language filepath componentAST
    where
        defFilename = justOrError "No default filename!" defaultFilename


-- writeEntity :: (MonadConfigEnvironment m, MonadIO m)
--             => EntityModel ast field
--             -> Entity
--             -> m ()
-- writeEntity EntityModel {..} entity = do
--     fn <- catchError m a e -> m a
--     fn <- justOrError "No default filename!" $ efn `mplus` mfn `mplus` fmap ($ name entity) defaultFilename
--     let fp = fromMaybe defaultPath $ ep `mplus` mp
--         filepath = fp ++ fn
--     liftIO $ prettyWrite language filepath (renderEntity $ emap mapToField entity)
--     where
--         mapToField n t = fromMaybe (defaultField t) (Map.lookup n fm `mplus` Map.lookup t tm)
















-- class EntityModel a ast field interface where
--     language :: Language ast
--     defaultField :: FieldType -> field
--     defaultFilename :: Maybe (Name -> String)
--     defaultFilename = Nothing

--     renderEntity :: EntityInfo field -> ast
--     renderInterface :: Entity -> interface
--     default renderInterface :: EntityInfo field -> ()
--     renderInterface _ = ()

--     dependencies

-- data EntityModelDependency interface
--     = forall a ast field. (EntityModel a ast field interface) => EntityModelDependency (EntityModel a ast field interface)

-- class StaticDependency a deps where
--     type StaticInterface a :: *
--     getStaticInterface :: a -> deps -> StaticInterface a

-- class FieldDependency a field where
--     type FieldInterface a :: *
--     getFieldInterface :: a -> [field] -> [FieldInterface a]

-- type family GatherStaticDependencies (xs :: [*]) :: [*] where
--     GatherStaticDependencies '[] = '[]
--     GatherStaticDependencies (x ': xs) = StaticInterface x ': GatherStaticDependencies xs

-- instance (AllHave2 StaticDependency ts ds) => StaticDependency (HVect ts) (HVect ds) where
--     type StaticInterface (HVect ts) = HVect (GatherStaticDependencies ts)
--     getStaticInterface HNil _ = HNil
--     getStaticInterface (x :&: xs) (d :&: ds) = getStaticInterface x d :&: getStaticInterface xs ds

-- type family GatherFieldDependencies (xs :: [*]) :: [*] where
--     GatherFieldDependencies '[] = '[]
--     GatherFieldDependencies (x ': xs) = FieldInterface x ': GatherFieldDependencies xs

-- instance (AllHave2 FieldDependency ts ds) => FieldDependency (HVect ts) (HVect ds) where
--     type FieldInterface (HVect ts) = HVect (GatherFieldDependencies ts)
--     getFieldInterface _ [] = []
--     getFieldInterface HNil (_:fs) = HNil : getFieldInterface HNil fs
--     getFieldInterface (x :&: xs) ((d :&: ds):fs) = (getFieldInterface x d :&: getFieldInterface xs ds) : getFieldInterface (x :&: xs) fs



-- class (Dependency deps) => EntityComponent a ast deps where
--     language :: a -> Language ast
--     renderComponent :: a -> deps -> ast
--     defaultFilename :: a -> Maybe (Name -> String)

-- class (EntityComponent a ast deps) => EntityModel a ast field deps where
--     defaultField :: a -> FieldType -> field

-- data EntityModel ast field deps = EntityModel (Language ast) (EntityInfo field -> ast) (Maybe (Name -> String)) (FieldType -> field)
-- defaultField :: EntityModel ast field deps -> FieldType -> field
-- defaultField (EntityModel _ _ _ df) = df

-- instance EntityComponent (EntityModel ast field deps) ast deps where
--     language (EntityModel lang _ _ _) = lang
--     renderComponent (EntityModel _ render _ _) = render
--     defaultFilename (EntityModel _ _ dfn _) = dfn


-- data EntityModel ast field deps
--     = EntityModel { language :: Language ast
--                   -- , defaultField :: FieldType -> field
--                   , renderComponent :: EntityInfo field -> ast
--                   , defaultFilename :: Maybe (Name -> String)
--                   }


-- data ModelConfiguration field = ModelConfiguration { filename :: Maybe String
--                                                    , path :: Maybe FilePath
--                                                    , typeMap :: Map.Map FieldType field}
-- instance Default (ModelConfiguration field) where
--     def = ModelConfiguration def def def


-- data EntityConfiguration field = EntityConfiguration { filename :: Maybe String
--                                                      , path :: Maybe FilePath
--                                                      , fieldMap :: Map.Map Name field }
-- instance Default (EntityConfiguration field) where
--     def = EntityConfiguration def def def

-- defaultPath :: FilePath
-- defaultPath = "./"

-- writeEntity :: (MonadError Error m, MonadIO m)
--             => EntityModel ast field
--             -> ModelConfiguration field
--             -> EntityConfiguration field
--             -> Entity
--             -> m ()
-- writeEntity EntityModel {..} (ModelConfiguration mfn mp tm) (EntityConfiguration efn ep fm) entity = do
--     fn <- justOrError "No default filename!" $ efn `mplus` mfn `mplus` fmap ($ name entity) defaultFilename
--     let fp = fromMaybe defaultPath $ ep `mplus` mp
--         filepath = fp ++ fn
--     liftIO $ prettyWrite language filepath (renderEntity $ emap mapToField entity)
--     where
--         mapToField n t = fromMaybe (defaultField t) (Map.lookup n fm `mplus` Map.lookup t tm)
