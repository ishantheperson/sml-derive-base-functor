{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SML.Syntax where 

import Data.Functor.Foldable.TH

data SMLType = 
    TypeVariable String 
  | TypeTuple [SMLType] -- ^ e.g. (int, string) either. Can have any number of elements (even 0 or 1)
  | TypeConstructor SMLType String -- ^ t1 t2 e.g. int list. Right side must be the name of a datatype
  | Function SMLType SMLType 
  | TupleType [SMLType] -- ^ e.g. (string * int) list. MUST have more than 1 element
  | RecursiveMarker -- ^ Indicates self reference in a datatype
  deriving (Show)

data SMLDatatype = SMLDatatype { 
    name :: String,
    typeVariables :: [String],
    cases :: [(String, Maybe SMLType)]
  } deriving (Show)

data SMLCaseArm = SMLCaseArm { 
    variantName :: String, 
    boundName :: Maybe String, 
    body :: SMLExpression 
  } deriving (Show)

data SMLExpression = 
    Case SMLExpression [SMLCaseArm]
  | Variable String 
  | Application SMLExpression SMLExpression
  -- | let (v1, v2) = e1 in e2 end
  | Let [String] SMLExpression SMLExpression
  deriving (Show)

data SMLFunction = SMLFunction {
    name :: String,
    param :: String,
    body :: SMLExpression
  } deriving (Show)

makeBaseFunctor ''SMLType 