{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module SML.Generator where

import Data.Functor.Foldable

import SML.Syntax
    ( SMLDatatype(..),
      SMLType(TypeVariable),
      SMLTypeF(RecursiveMarkerF),
      SMLFunction(..),
      SMLExpression(Variable, Application, Case),
      SMLCaseArm(..) )

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

makeProject :: SMLDatatype -> SMLFunction 
makeProject datatype = 
  let 
    name = "project"
    param = "it"

    body = Case (Variable "it") (map projectCase (cases datatype))
  in 
    SMLFunction{..} 
  where 
    projectCase :: (String, Maybe SMLType) -> SMLCaseArm
    projectCase (variantName, Nothing) = 
      let 
        boundName = Nothing 
        body = Variable (variantName <> "F")
      in SMLCaseArm {..}

    projectCase (variantName, Just _) = 
      let 
        boundName = Just "v"
        body = Application (Variable (variantName <> "F")) (Variable "v")
      in SMLCaseArm {..}

makeEmbed :: SMLDatatype -> SMLFunction
makeEmbed datatype = 
  let 
    name = "project"
    param = "it"

    body = Case (Variable "it") (map projectCase (cases datatype))
  in 
    SMLFunction{..} 
  where 
    projectCase :: (String, Maybe SMLType) -> SMLCaseArm
    projectCase (variantName, Nothing) = 
      let 
        boundName = Nothing 
        body = Variable variantName
      in SMLCaseArm (variantName <> "F") boundName body

    projectCase (variantName, Just _) = 
      let 
        boundName = Just "v"
        body = Application (Variable variantName) (Variable "v")
      in SMLCaseArm (variantName <> "F") boundName body

makeMap :: SMLDatatype -> SMLFunction 
makeMap SMLDatatype {cases} =
  let 
  in undefined