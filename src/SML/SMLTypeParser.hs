{-# LANGUAGE RecordWildCards #-}
module SML.SMLTypeParser where 

import Control.Monad (void)

import Control.Arrow
import Data.Functor.Foldable

import Text.Megaparsec 
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad.Combinators.Expr
import Data.Void 

import SML.SMLDatatype 

type Parser = Parsec Void String 

smlDatatype :: Parser SMLDatatype 
smlDatatype = do 
  reserved "datatype"
  typeVariables <- typeParams
  name <- identifier
  symbol "="

  -- Every time this datatype is used recursively, replace it with a marker.
  -- This is not perfect, for example:
  -- datatype 'a foo = X of int foo | Y of 'a foo
  -- Only the Y case should be altered but this will
  -- change X too.
  -- This could be fixed by making sure that the argument to the type constructor
  -- is the same type tuple but that gets annoying and shouldn't really
  -- be a problem
  let replacedRecursive (TypeConstructorF _ typeName) | typeName == name = RecursiveMarker
      replacedRecursive other = embed other

  cases <- sepBy1 (second (fmap (cata replacedRecursive)) <$> dataCase) (symbol "|")

  return SMLDatatype {..}
  
  where dataCase :: Parser (String, Maybe SMLType)
        dataCase = do 
          name <- identifier 
          associatedData <- optional (reserved "of" *> smlType)
          
          return (name, associatedData)
          
        typeParams = 
              parens (sepBy typeVariable (symbol ","))
          <|> pure <$> typeVariable
          <|> return []


-- Really only used when parsing data types
smlType :: Parser SMLType
smlType = makeExprParser (term >>= postfixA) operators <?> "type"
  where term =  
              TypeVariable <$> typeVariable 
          <|> TypeConstructor (TypeTuple []) <$> identifier
          <|> typeVarTuple 
          <|> parens smlType 
        
        -- A little hacky, but whats happening is that
        -- we need to know if we are parsing a tuple
        -- type or not. In postfixA we aren't necessarily,
        -- but in postfixB we are.
        postfixA e = tupleType e <|> typeApp e postfixA <|> return e
        postfixB e = typeApp e postfixB <|> return e
        tupleType e = do 
          ts <- some (symbol "*" *> (term >>= postfixB))  
          return $ TupleType (e:ts)

        typeApp e next = identifier >>= \name -> next (TypeConstructor e name)

        operators = [[InfixR (Function <$ symbol "->")]]

        typeVarTuple = try $ parens $ do  
          tvs <- sepBy smlType (symbol ",")
          return $ case tvs of 
                    [t] -> t
                    _ -> TypeTuple tvs 
        
typeVariable :: Parser String
typeVariable = char '\'' *> identifier <?> "type variable"

identifier :: Parser String
identifier = (lexeme . try) (p >>= check) <?> "identifier"
  where p = (:) <$> identStart <*> many identLetter
        identStart = letterChar
        identLetter = alphaNumChar <|> char '_' <|> char '\'' <|> char '.'

        check x = if x `elem` reservedWords
                    then fail $ "'" ++ x ++ "' is a reserved word and cannot be an identifier"
                    else return x
      
lexeme, parens :: Parser a -> Parser a 
lexeme = Lex.lexeme sc 
parens = between (try $ symbol "(" <* notFollowedBy (char '*')) (symbol ")")

symbol :: String -> Parser String
symbol = Lex.symbol sc

reserved :: String -> Parser ()
reserved word = (lexeme . try) (string word *> notFollowedBy alphaNumChar)

-- | Non-exhausive list of SML reserved words
reservedWords :: [String]
reservedWords = ["val",
                 "fun",
                 "if",
                 "then",
                 "else",
                 "fn",
                 "let",
                 "in",
                 "end",
                 "case",
                 "of",
                 "datatype"]


sc, blockComment :: Parser () 
sc = Lex.space space1 empty blockComment

blockComment = do 
  void $ try (string "(*" <* notFollowedBy (char '@'))
  void $ manyTill anySingle (string "*)")
