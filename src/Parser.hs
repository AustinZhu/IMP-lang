module Parser where

import Lexer
import Syntax
import Text.ParserCombinators.Parsec (Parser, sepBy1, (<|>))
import Text.ParserCombinators.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)

impParser :: Parser Com
impParser = space >> coms

aExp :: Parser Aexp
aExp = buildExpressionParser aOp aTerm
  where
    aOp =
      [ [Infix (operator "-" >> return Sub) AssocLeft],
        [Infix (operator "+" >> return Add) AssocLeft],
        [Infix (operator "*" >> return Mul) AssocLeft]
      ]
    aTerm =
      parentheses aExp
        <|> fmap Loc location
        <|> fmap N number

bExp :: Parser Bexp
bExp = buildExpressionParser bOp bTerm
  where
    bOp =
      [ [Prefix (operator "!" >> return Not)],
        [Infix (operator "&" >> return And) AssocLeft],
        [Infix (operator "|" >> return Or) AssocLeft]
      ]
    bTerm =
      parentheses bExp
        <|> (keyword "true" >> return (T True))
        <|> (keyword "false" >> return (T False))
        <|> do a1 <- aExp; operator "="; Eq a1 <$> aExp
        <|> do a1 <- aExp; operator "<="; Leq a1 <$> aExp

coms :: Parser Com
coms = parentheses coms <|> seqCom
  where
    com = ifCom <|> whileCom <|> skipCom <|> defCom
      where
        ifCom = do
          keyword "if"
          predicate <- bExp
          keyword "then"
          a1 <- aExp
          keyword "else"
          If predicate a1 <$> aExp
        whileCom = do
          keyword "while"
          predicate <- bExp
          keyword "do"
          While predicate <$> aExp
        defCom = do
          loc <- location
          operator ":="
          Def loc <$> aExp
        skipCom = keyword "skip" >> return Skip
    seqCom = parentheses seqCom <|> do cs <- sepBy1 com semicolon; return $ if length cs == 1 then head cs else Seq cs
