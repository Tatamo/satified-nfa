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

type AndTerm =
  | Literal of Literal
  | And of Seq<Literal>

type DNF = 
  | AndTerm of AndTerm
  | Or of Seq<AndTerm>

type OrTerm =
  | Literal of Literal
  | Or of Seq<Literal>

type CNF = 
  | OrTerm of OrTerm
  | And of Seq<OrTerm>
