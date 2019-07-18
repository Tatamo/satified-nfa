module Logic

type Atomic =
  | True
  | False
  | Var of string

type Literal = 
  | Atomic of Atomic
  | Not of Atomic

type SemiLiteral =
  | Literal of Literal
  | Not of SemiLiteral

type Term =
  | Literal of Literal
  | And of Literal * Literal
  | Or of Literal * Literal
