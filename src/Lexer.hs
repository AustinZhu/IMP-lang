module Lexer where

import Text.Parsec.Char (alphaNum, letter)
import Text.Parsec.Token
  (GenTokenParser,  GenLanguageDef (..),
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
  ( GenLanguageDef (identLetter, identStart, reservedNames, reservedOpNames),
    emptyDef,
  )
import Text.Parsec (ParsecT)
import Data.Functor.Identity (Identity)

langDef :: GenLanguageDef String u Identity
langDef =
  emptyDef
    { identStart = letter,
      identLetter = alphaNum,
      reservedNames = ["true", "false", "skip", "if", "then", "else", "while", "do"],
      reservedOpNames = ["+", "-", "*", "=", "<=", "!", "&", "|", "::="]
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
