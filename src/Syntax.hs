module Syntax
  ( Aexp (..),
    Bexp (..),
    Com (..),
  )
where

data Aexp = N Integer | Loc String | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp deriving (Show)

data Bexp = T Bool | Eq Aexp Aexp | Leq Aexp Aexp | Not Bexp | And Bexp Bexp | Or Bexp Bexp deriving (Show)

data Com = Skip | Let String Aexp | Seq [Com] | If Bexp Com Com | While Bexp Com deriving (Show)
