{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SML.Print where

import Data.Maybe
import Data.List (intercalate) 
import Data.Functor.Foldable

import qualified Data.Text as T
import Text.Printf
import Prettyprinter

import SML.Syntax 

printType :: SMLType -> String 
printType = cata go 
  where
    go :: SMLTypeF String -> String
    go (TypeVariableF t) = "'" <> t 
    go (TypeConstructorF [] t) = t 
    go (TypeConstructorF ts t) = printf "(%s) %s" (intercalate ", " ts) t 
    go (FunctionF a b) = printf "(%s) -> (%s)" a b
    go (TupleTypeF ts) = intercalate " * " ts
    go RecursiveMarkerF = error "Recursive marker should not show up"

barSeparated :: [Doc ann] -> Doc ann
barSeparated = align . encloseSep (flatAlt "  " mempty) mempty (flatAlt "| " " | ")

printDataType :: SMLDatatype -> Doc ann
printDataType SMLDatatype{..} = group $ 
  let 
    typevarRep :: String
    typevarRep = if null typeVariables then "" else printf "(%s)" (intercalate "," (("'" <>)<$> typeVariables))
  in 
      hang 2 $ sep [
        hsep ["datatype", pretty typevarRep, pretty name, "="],
        barSeparated (pretty . printCase <$> cases)
      ]
  where 
    printCase :: (String, Maybe SMLType) -> String 
    printCase (variantName, Nothing) = variantName
    printCase (variantName, Just typ) = printf "%s of %s" variantName (printType typ)


printCaseArm :: SMLCaseArm -> Doc ann
printCaseArm SMLCaseArm{..} = group $ hang 2 $
  case boundName of 
    Just vname -> pretty variantName <+> pretty vname <+> "=>" <> line <> printExpr body 
    Nothing -> pretty variantName <+> "=>" <> softline <> printExpr body 

printExpr :: SMLExpression -> Doc ann 
printExpr (Case obj arms) = hang 2
  ("case" <+> printExpr obj <+> "of" <> line <> barSeparated (printCaseArm <$> arms))
printExpr (Variable s) = pretty s 
printExpr (Application e1 e2) = parens (printExpr e1) <+> parens (printExpr e2)
printExpr (MakeTuple es) = tupled (printExpr <$> es)
printExpr (LetTuple vs e1 e2) = align $
  sep [hang 2 ("let" <> line <> ("val" <+> tupled (pretty <$> vs) <+> "=" <+> printExpr e1)),
       hang 2 ("in" <> line <> printExpr e2),
       "end"]
printExpr (LetFunction func e) = align $
  sep [hang 2 ("let" <> line <> printFunction func),
       hang 2 ("in" <> line <> printExpr e),
       "end"]

printFunction :: SMLFunction -> Doc ann
printFunction SMLFunction{..} = hang 2 $
  "fun" <+> pretty name <+> hsep (pretty <$> params) <+> "=" <> line <> printExpr body
