{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SML.Syntax where 

import Data.Functor.Foldable.TH

data SMLType = 
    TypeVariable String 
  | TypeConstructor [SMLType] String -- ^ t1 t2 e.g. int list. Right side must be the name of a datatype
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
  -- | (e1, e2, ..)
  | MakeTuple [SMLExpression]
  -- | let (v1, v2) = e1 in e2 end.
  -- Also works for single variable bindings
  | LetTuple [String] SMLExpression SMLExpression
  -- | let fun f x = .. in e2 end
  | LetFunction SMLFunction SMLExpression
  deriving (Show)

infixl `Application`

data SMLFunction = SMLFunction {
    name :: String,
    params :: [String],
    body :: SMLExpression
  } deriving (Show)

makeBaseFunctor ''SMLType 