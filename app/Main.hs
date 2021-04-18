{-# LANGUAGE TemplateHaskell #-}
module Main where

import Entity.Languages.Python
import Entity.Entity
import Entity.Core
import Data.HVect

$(mkEntity
    "event"
    [ ("id", IntegerField)
    , ("name", StringField)
    , ("date", DateField)
    , ("description", StringField)
    ])


main :: IO ()
main = generateStructure interfaces providers
    where
        providers = 
                djangoModel Event
            :%: djangoRepo Event
            :%: djangoRESTView Event
            :%: PNil

        interfaces =
                mkPythonModel Event "event.models"
            :&: mkPythonRepo Event "event.models"
            :&: mkPythonREST Event "event.views"
            :&: mkREST Event "api/events/"
            :&: HNil
