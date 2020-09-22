module Syntax
    ( Aexp,
      Bexp,
      Com
    ) where

data Aexp = N Int | Loc String | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp deriving (Show)

data Bexp = T Bool | Eq Aexp Aexp | Leq Aexp Aexp | Neg Bexp | And Bexp Bexp | Or Bexp Bexp deriving (Show)

data Com = Skip | Def String Aexp | Seq [Com] | If Bexp Aexp Aexp | While Bexp Aexp deriving (Show)
