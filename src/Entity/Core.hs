{-# LANGUAGE TypeSynonymInstances #-}
module Entity.Core where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Except
import Entity.Util
import Data.Default


type Error = String
    

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


data EntityModel ast field
    = EntityModel { language :: Language ast
                  , defaultField :: FieldType -> field
                  , renderEntity :: EntityInfo field -> ast
                  , defaultFilename :: Maybe (Name -> String)
                  }


data ModelConfiguration field = ModelConfiguration { filename :: Maybe String
                                                   , path :: Maybe FilePath
                                                   , typeMap :: Map.Map FieldType field}
instance Default (ModelConfiguration field) where
    def = ModelConfiguration def def def


data EntityConfiguration field = EntityConfiguration { filename :: Maybe String
                                                     , path :: Maybe FilePath
                                                     , fieldMap :: Map.Map Name field }
instance Default (EntityConfiguration field) where
    def = EntityConfiguration def def def

defaultPath :: FilePath
defaultPath = "./"

writeEntity :: (MonadError Error m, MonadIO m)
            => EntityModel ast field
            -> ModelConfiguration field
            -> EntityConfiguration field
            -> Entity
            -> m ()
writeEntity EntityModel {..} (ModelConfiguration mfn mp tm) (EntityConfiguration efn ep fm) entity = do
    fn <- justOrError "No default filename!" $ efn `mplus` mfn `mplus` fmap ($ name entity) defaultFilename
    let fp = fromMaybe defaultPath $ ep `mplus` mp
        filepath = fp ++ fn
    liftIO $ prettyWrite language filepath (renderEntity $ emap mapToField entity)
    where
        mapToField n t = fromMaybe (defaultField t) (Map.lookup n fm `mplus` Map.lookup t tm)
