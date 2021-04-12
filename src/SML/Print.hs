{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module SML.Print where


import Data.List 
import Data.Functor.Foldable
import Control.Monad.State.Strict
import Control.Monad.State.Class

import Text.Printf 

import SML.Syntax 

data SMLPrinterState = SMLPrinterState { indentLevel :: Int, writtenCode :: [String] }

initialSMLPrinterState :: SMLPrinterState
initialSMLPrinterState = SMLPrinterState 0 []

type PrintSML = MonadState SMLPrinterState

runSMLPrinter :: State SMLPrinterState a -> String
runSMLPrinter = unlines . reverse . writtenCode . flip execState initialSMLPrinterState

indent, unindent :: PrintSML m => m () 
indent = do 
  state <- get 
  put $ state { indentLevel = indentLevel state + 1 }

unindent = do 
  state <- get 
  put $ state { indentLevel = indentLevel state - 1 }

outputLine :: PrintSML m => String -> m () 
outputLine s = do 
  state <- get 
  let indentAmount = spacesPerIndent * indentLevel state 
      line = replicate indentAmount ' ' ++ s 
  put (state { writtenCode = line : writtenCode state })
  where 
    spacesPerIndent :: Int 
    spacesPerIndent = 4

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

printDatatype :: SMLDatatype -> String 
printDatatype SMLDatatype{..} = runSMLPrinter $ do
  let typevarRep = if null typeVariables then "" else printf "(%s)" (intercalate "," (("'" <>)<$> typeVariables))
  outputLine $ printf "datatype %s %s =" typevarRep name 

  let (first:others) = printCase <$> cases 

  indent
  outputLine $ printf "  %s" first 
  forM_ others $ \other -> outputLine $ printf "| %s" other

  where 
    printCase :: (String, Maybe SMLType) -> String 
    printCase (variantName, Nothing) = variantName
    printCase (variantName, Just typ) = printf "%s of %s" variantName (printType typ)

