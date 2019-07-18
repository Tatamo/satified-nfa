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
  | SemiLiteral of SemiLiteral
  | And of Seq<Term>
  | Or of Seq<Term>
  | Not of Term
