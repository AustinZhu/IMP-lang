module Lexer where

import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token (GenLanguageDef, makeTokenParser, identifier, reserved, reservedOp, parens, whiteSpace, semi, integer)
import Text.Parsec.Char (letter, alphaNum)

langDef = emptyDef { 
  identStart = letter,
  identLetter = alphaNum,
  reservedNames = ["true", "false", "skip", "if", "then", "else", "while", "do"],
  reservedOpNames = ["+", "-", "*", "=", "<=", "!", "&", "|", "::=", ]
}

lexer = makeTokenParser langDef

location = identifier lexer

keyword = reserved lexer

operator = reservedOp lexer

space = whiteSpace lexer

semicolon = semi lexer

parentheses = parens lexer

number = integer lexer