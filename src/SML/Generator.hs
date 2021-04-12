{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module SML.Generator where

import Data.Char

import Data.Functor.Foldable

import SML.Syntax

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
    params = ["it"]

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
makeEmbed SMLDatatype {cases} = 
  let 
    name = "project"
    params = ["it"]

    body = Case (Variable "it") (map projectCase cases)
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
        body = Variable variantName `Application` Variable "v"
      in SMLCaseArm (variantName <> "F") boundName body

makeMap :: SMLDatatype -> SMLFunction 
makeMap SMLDatatype {cases} =
  let 
    name = "map"
    params = ["f", "it"]
    body = Case (Variable "it") (map makeCaseArm cases)
  in SMLFunction{..}
  where 
    makeCaseArm :: (String, Maybe SMLType) -> SMLCaseArm
    makeCaseArm (variantName, Nothing) = SMLCaseArm variantName Nothing (Variable variantName)
    makeCaseArm (variantName, Just typ) = 
      SMLCaseArm variantName (Just "v") $ (Variable variantName) `Application` mapExpr (Variable "v") typ

    mapExpr :: SMLExpression -> SMLType -> SMLExpression 
    mapExpr v (TypeVariable t) | t == recursionVariableName = Application (Variable "f") v
                               | otherwise = v 
    mapExpr v (TypeConstructor [] _) = v
    mapExpr v (TypeConstructor [paramType] constructor) = 
      let 
        moduleName = toUpper (head constructor) : tail constructor
        func = Variable $ moduleName <> ".map"
        mapper = SMLFunction {
            name = "g",
            params = ["v"],
            body = mapExpr (Variable "v") paramType
          }
      in 
        LetFunction mapper $ func `Application` Variable "g" `Application` v

    mapExpr v (TypeConstructor _ _) = error "Don't know how to deal with multiple tyvars"
    mapExpr v (Function _ _) = error "Don't want to deal with function types right now"
    mapExpr v (TupleType tys) = 
      let 
        names = zipWith (\i _ -> "v" <> show i) [1..] tys 
        es = zipWith mapExpr (Variable <$> names) tys 
      in 
        LetTuple names v (MakeTuple es)

    mapExpr _ RecursiveMarker = error "Impossible"
