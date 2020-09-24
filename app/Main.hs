module Main where

import Control.Monad (unless)
import Eval (emptyState, evalCom, toString)
import GHC.IO.Handle (hFlush)
import Parser (parseIMP)
import System.IO (stdout)

main :: IO ()
main = do
  putStr "IMP> "
  hFlush stdout
  code <- getLine
  unless (code == "\\q") $ do
    let ast = parseIMP code
    let state = evalCom (ast, emptyState)
    putStrLn $ toString state
    main
