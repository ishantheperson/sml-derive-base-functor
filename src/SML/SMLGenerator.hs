{-# LANGUAGE RecordWildCards #-}
module SML.SMLGenerator where

import Data.Functor.Foldable

import SML.SMLDatatype

recursionVariableName = "self"

-- | Creates the datatype declaration for the base functor
baseFunctor :: SMLDatatype -> SMLDatatype
baseFunctor SMLDatatype{..} = 
  let 
    functorName = name <> "F"
    functorTypeParams = recursionVariableName : typeVariables 
    functorDataCases = map baseFunctorify cases

  in 
    SMLDatatype functorName functorTypeParams functorDataCases
  where 
    removeRecursion :: SMLTypeF SMLType -> SMLType
    removeRecursion RecursiveMarkerF = TypeVariable recursionVariableName
    removeRecursion other = embed other 

    baseFunctorify :: (String, Maybe SMLType) -> (String, Maybe SMLType)
    baseFunctorify (name, maybeType) = (name <> "F", cata removeRecursion <$> maybeType)

makeProject :: SMLDatatype -> String

