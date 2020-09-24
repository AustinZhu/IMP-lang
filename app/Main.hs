module Main where

import Parser (impParser)
import Syntax (Com)
import Text.Parsec.Prim (parse)

parseIMP :: String -> Com
parseIMP str = case parse impParser "" str of
  Left e -> error $ show e
  Right r -> r

main :: IO String
main = do
  ast <- parseIMP <$> getLine
  return $ show ast
