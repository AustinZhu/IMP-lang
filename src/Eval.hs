module Eval
  ( emptyState,
    toString,
    evalCom,
  )
where

import Data.Map (Map, empty, insert, lookup, toList)
import Syntax
import Prelude hiding (lookup)

type State = Map String Integer

emptyState :: Map String Integer
emptyState = empty

toString :: State -> String
toString s = foldl (\acc x -> fst x ++ ": " ++ show (snd x) ++ "\n" ++ acc) "" $ toList s

evalAexp :: (Aexp, State) -> Integer
evalAexp (N i, _) = i
evalAexp (Loc str, s) = case lookup str s of
  Nothing -> error "variable not in scope"
  Just i -> i
evalAexp (Add a1 a2, s) = evalAexp (a1, s) + evalAexp (a2, s)
evalAexp (Sub a1 a2, s) = evalAexp (a1, s) - evalAexp (a2, s)
evalAexp (Mul a1 a2, s) = evalAexp (a1, s) * evalAexp (a2, s)

evalBexp :: (Bexp, State) -> Bool
evalBexp (T b, _) = b
evalBexp (Eq a1 a2, s) = evalAexp (a1, s) == evalAexp (a2, s)
evalBexp (Leq a1 a2, s) = evalAexp (a1, s) <= evalAexp (a2, s)
evalBexp (Not b, s) = not $ evalBexp (b, s)
evalBexp (And b1 b2, s) = evalBexp (b1, s) && evalBexp (b2, s)
evalBexp (Or b1 b2, s) = evalBexp (b1, s) && evalBexp (b2, s)

evalCom :: (Com, State) -> State
evalCom (Skip, s) = s
evalCom (Let var a, s) = insert var (evalAexp (a, s)) s
evalCom (Seq cs, s) = foldl (\acc x -> evalCom (x, acc)) s cs
evalCom (If b c1 c2, s) = if evalBexp (b, s) then evalCom (c1, s) else evalCom (c2, s)
evalCom (While b c, s) = if evalBexp (b, s) then evalCom (While b c, evalCom (c, s)) else s
