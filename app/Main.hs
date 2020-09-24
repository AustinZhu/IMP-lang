module Main where

import Control.Monad (unless)
import GHC.IO.Handle (hFlush)
import Parser (impParser)
import Syntax (Com)
import System.IO (stdout)
import Text.Parsec.Prim (parse)

parseIMP :: String -> Com
parseIMP str = case parse impParser "" str of
  Left e -> error $ show e
  Right r -> r

main :: IO ()
main = do
  putStr "IMP> "
  hFlush stdout
  code <- getLine
  unless (code == "\\q") $ do
    print $ parseIMP code
    main
