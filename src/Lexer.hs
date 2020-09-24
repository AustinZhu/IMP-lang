module Lexer where

import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT)
import Text.Parsec.Char (alphaNum, letter)
import Text.Parsec.Token
  ( GenLanguageDef (..),
    GenTokenParser,
    identifier,
    integer,
    makeTokenParser,
    parens,
    reserved,
    reservedOp,
    semi,
    whiteSpace,
  )
import Text.ParserCombinators.Parsec.Language
  ( emptyDef,
  )

langDef :: GenLanguageDef String u Identity
langDef =
  emptyDef
    { identStart = letter,
      identLetter = alphaNum,
      reservedNames = ["true", "false", "skip", "if", "then", "else", "while", "do"],
      reservedOpNames = ["+", "-", "*", "=", "<=", "!", "&", "|", ":="]
    }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser langDef

location :: ParsecT String u Identity String
location = identifier lexer

keyword :: String -> ParsecT String u Identity ()
keyword = reserved lexer

operator :: String -> ParsecT String u Identity ()
operator = reservedOp lexer

space :: ParsecT String u Identity ()
space = whiteSpace lexer

semicolon :: ParsecT String u Identity String
semicolon = semi lexer

parentheses :: ParsecT String u Identity a -> ParsecT String u Identity a
parentheses = parens lexer

number :: ParsecT String u Identity Integer
number = integer lexer
