module Main where

import Entity.Languages.Python
import Entity.Languages.TypeScript
import Entity.Core
import Entity.Util
import Control.Monad.Except
import Entity.ComponentConfig
import Data.Default ( Default(def) )
import Control.Monad.Except (withExceptT, liftEither, runExceptT)
import qualified Data.Map as Map


n :: String -> Name
n = fromKebab

eventEntity :: Entity
eventEntity = Entity 
    { name = n "event"
    , fields = [(n "name", StringField), (n "year", IntegerField), (n "description", StringField)]
    }

eventDjangoConfig :: ComponentConfig
eventDjangoConfig = Map.fromList [("typeMap", ConfigListField [])]

configMap :: ConfigMap
configMap = Map.fromList [("Django:event", eventDjangoConfig)]

-- eventDjangoConfig :: EntityConfiguration DjangoField
-- eventDjangoConfig = EntityConfiguration def def (Map.singleton (n "description") DjangoTextField)

writeComponentIO :: ConfigMap -> Component ast (ConfigEnvironment Error) -> IO ()
writeComponentIO config component = do
    res <- liftEither $ runExcept $ withExcept userError (runConfigEnvironment (writeComponent component) config)
    res


main :: IO ()
main = do
    writeComponentIO configMap (djangoModel eventEntity)
    writeComponentIO configMap (typeScriptModel eventEntity)
