module Main where

import Entity.Languages.Python
import Entity.Languages.TypeScript
import Entity.Core
import Entity.Util
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

eventDjangoConfig :: EntityConfiguration DjangoField
eventDjangoConfig = EntityConfiguration def def (Map.singleton (n "description") DjangoTextField)

writeEntityIO :: EntityModel ast field -> EntityConfiguration field -> Entity -> IO ()
writeEntityIO model entityConfig entity = do
    res <- runExceptT $ withExceptT userError (writeEntity model def entityConfig entity)
    liftEither res


main :: IO ()
main = do
    writeEntityIO djangoModel eventDjangoConfig eventEntity
    writeEntityIO typeScriptModel def eventEntity
