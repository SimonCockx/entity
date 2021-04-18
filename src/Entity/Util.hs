module Entity.Util where

import Control.Monad.Except ( MonadError(throwError) )
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.HVect (HVect (HNil, (:&:)), SNat (..), Nat (..), HVectIdx, InList, Nat)
import qualified Data.HVect as H


type Error = String


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



type family AllInList (as :: [*]) (bs :: [*]) :: [Nat] where
    AllInList '[] bs = '[]
    AllInList (a ': as) bs = InList a bs ': AllInList as bs

type SublistContains ns xs ts = (SNatsRep ns, AllInList xs ts ~ ns, HVectIdxs ns ts ~ xs)

data SNats ns where
    NilNats :: SNats '[]
    ConsNats :: SNat n -> SNats ns -> SNats (n ': ns)

class SNatRep n where
    getSNat :: SNat n

instance SNatRep 'Zero where
    getSNat = SZero

instance SNatRep n => SNatRep ('Succ n) where
    getSNat = SSucc getSNat

class SNatsRep ns where
    getSNats :: SNats ns

instance SNatsRep '[] where
    getSNats = NilNats

instance (SNatsRep ns, SNatRep n) => SNatsRep (n ': ns) where
    getSNats = ConsNats getSNat getSNats

type family HVectIdxs (ns :: [Nat]) (ts :: [*]) :: [*] where
    HVectIdxs '[] ts = '[]
    HVectIdxs (n ': ns) ts = HVectIdx n ts ': HVectIdxs ns ts

(!!!) :: SNats ns -> HVect as -> HVect (HVectIdxs ns as)
NilNats !!! _ = HNil
(ConsNats n ns) !!! as = (n H.!! as) :&: (ns !!! as)

infixl 9 !!!

findFirsts :: forall ts us ns. (SublistContains ns us ts) => HVect ts -> HVect us
findFirsts vect = idxs !!! vect
    where
        idxs :: SNats ns
        idxs = getSNats
