module Entity.Util where

import Control.Monad.Except ( MonadError(throwError) )
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)

type Name = [String]

justOrError :: (MonadError e m) => e -> Maybe a -> m a
justOrError e = maybe (throwError e) return

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

toPascal :: Name -> String
toPascal = concatMap capitalize

toCamel :: Name -> String
toCamel [] = ""
toCamel (x:xs) = x ++ toPascal xs

toKebab :: Name -> String
toKebab = intercalate "-"

fromKebab :: String -> Name
fromKebab = splitOn "-"

toSnake :: Name -> String
toSnake = intercalate "_"
