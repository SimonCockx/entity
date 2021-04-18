{-# LANGUAGE TemplateHaskell #-}
module Entity.RefactorTest where

import Data.HVect (HVect (HNil, (:&:)))
import Entity.Util
import Entity.RefactoredEntity


-- Base types
newtype Language ast = Language { prettyWrite :: FilePath -> ast -> IO () }

data ComponentGenerator ast requires = ComponentGenerator (Language ast) (HVect requires -> (ast, FilePath))

data Providers interfaces iss nss where
    PNil :: Providers interfaces '[] '[]
    (:%:) :: forall ast ns nss is iss interfaces. (SublistContains ns is interfaces) => ComponentGenerator ast is -> Providers interfaces iss nss -> Providers interfaces (is ': iss) (ns ': nss)

infixr 5 :%:

generateStructure :: HVect interfaces -> Providers interfaces iss nss -> IO ()
generateStructure _ PNil = return ()
generateStructure interfaces ((ComponentGenerator generate) :%: gens) = do
    generate (findFirsts interfaces)
    generateStructure interfaces gens


-- Example

data PythonModelInterface e = PythonModelInterface e String

data PeriodPythonRepoInterface
    = PeriodPythonRepoInterface { modulePath :: String, repoGetName :: String }

moduleToFile :: String -> FilePath
moduleToFile m = map (\c -> if c == '.' then '/' else c) m ++ ".py"

periodDjangoRepoGenerator :: ComponentGenerator '[PeriodPythonRepoInterface]
periodDjangoRepoGenerator = ComponentGenerator (\(repoInterface :&: HNil) -> writeFile (moduleToFile $ modulePath repoInterface) "class PeriodRepo: pass")

$(mkEntity "my-entity" [("test", StringField), ("dees-is-lit", DateField)])
