module Main where

import Eval (State, emptyState, evalCom, printST)
import GHC.IO.Handle (hFlush)
import Parser (parseIMP)
import System.Exit (exitSuccess)
import System.IO (stdout)

main :: IO State
main = do
  putStrLn "IMP 1.0.0"
  putStrLn "Type \"syntax\" for more information."
  repl (pure emptyState)

repl :: IO State -> IO State
repl s = do
  putStr "IMP> "
  hFlush stdout
  code <- getLine
  if code == "syntax"
    then
      putStrLn
        "a ::= n | X | a + a | a - a | a * a \n\
        \b ::= true | false | a = a | a <= a | !b | b & b | b | b \n\
        \c ::= skip | X := a | c;c | if b then c else c | while b do c\n"
        >> hFlush stdout
        >> repl s
    else
      if code == "\\q"
        then exitSuccess
        else do
          let ast = parseIMP code
          prevState <- s
          let newState = evalCom (ast, prevState)
          putStrLn $ printST newState
          repl (pure newState)
