module Entity.ComponentConfig where

import Entity.Util
import Data.Map as M
import Control.Monad.Except ( MonadError(throwError), Except )
import Control.Monad.Reader


data ConfigField
    = ConfigIntegerField Integer
    | ConfigStringField String
    | ConfigListField [ConfigField]

fieldTypeName :: ConfigField -> String
fieldTypeName (ConfigIntegerField _) = "integer"
fieldTypeName (ConfigStringField _) = "string"
fieldTypeName (ConfigListField _) = "list"

throwUnexpectedFieldType :: (MonadError Error m) => String -> ConfigField -> m a
throwUnexpectedFieldType expected found = throwError $ "Unexpected field type. Expected " ++ expected ++ ", found " ++ fieldTypeName found ++ "."

extractInteger :: (MonadError Error m) => ConfigField -> m Integer
extractInteger (ConfigIntegerField i) = return i
extractInteger f = throwUnexpectedFieldType "integer" f

extractString :: (MonadError Error m) => ConfigField -> m String
extractString (ConfigStringField s) = return s
extractString f = throwUnexpectedFieldType "string" f

extractList :: (MonadError Error m) => (ConfigField -> m a) -> ConfigField -> m [a]
extractList extract (ConfigListField innerFields) = mapM extract innerFields
extractList _ f = throwUnexpectedFieldType "list" f


type ComponentConfig = Map String ConfigField

throwFieldNotFound :: (MonadError Error m) => String -> m a
throwFieldNotFound name = throwError $ "Field " ++ name ++ " not found."

getField :: (MonadError Error m) => (ConfigField -> m a) -> ComponentConfig -> String -> m a
getField extract con name = do
    field <- maybe (throwFieldNotFound name) return $ M.lookup name con
    extract field

type ConfigMap = Map String ComponentConfig

type ConfigEnvironment e = ReaderT ConfigMap (Except e)

class (MonadError Error m) => MonadConfigEnvironment m where
    getConfig :: String -> m ComponentConfig

    getInteger :: String -> String -> m Integer
    getInteger configName fieldName = do
        config <- getConfig configName
        getField extractInteger config fieldName
    getString :: String -> String -> m String
    getString configName fieldName = do
        config <- getConfig configName
        getField extractString config fieldName
    getList :: (ConfigField -> m a) -> String -> String -> m [a]
    getList extract configName fieldName = do
        config <- getConfig configName
        getField (extractList extract) config fieldName

throwConfigNotFound :: (MonadError Error m) => String -> m a
throwConfigNotFound name = throwError $ "Configuration " ++ name ++ " not found."

instance MonadConfigEnvironment (ConfigEnvironment Error) where
    getConfig name = do
        conMap <- ask
        case M.lookup name conMap of
            (Just config) -> return config
            Nothing       -> throwConfigNotFound name

runConfigEnvironment :: ConfigEnvironment Error a -> ConfigMap -> Except Error a
runConfigEnvironment = runReaderT
