{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
module SML.SMLDatatype where 

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

makeBaseFunctor ''SMLType 