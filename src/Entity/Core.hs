module Entity.Core where

import Entity.Util
import Data.HVect (HVect)
import Data.Set as S
import Control.Monad
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)


newtype Language ast = Language { prettyWrite :: FilePath -> ast -> IO () }

data ComponentGenerator ast requires = ComponentGenerator (Language ast) (HVect requires -> (FilePath, ast))

data Providers interfaces iss nss where
    PNil :: Providers interfaces '[] '[]
    (:%:) :: forall ast ns nss is iss interfaces. (SublistContains ns is interfaces) => ComponentGenerator ast is -> Providers interfaces iss nss -> Providers interfaces (is ': iss) (ns ': nss)

infixr 5 :%:

generateStructure :: HVect interfaces -> Providers interfaces iss nss -> IO ()
generateStructure = _generateStructure S.empty

_generateStructure :: S.Set FilePath -> HVect interfaces -> Providers interfaces iss nss -> IO ()
_generateStructure _ _ PNil = return ()
_generateStructure set interfaces ((ComponentGenerator lang generate) :%: gens) = do
    let (fp, ast) = generate (findFirsts interfaces)
    unless (S.member fp set) $ do
        createDirectoryIfMissing True $ takeDirectory fp
        writeFile fp ""
    prettyWrite lang fp ast
    _generateStructure (S.insert fp set) interfaces gens
