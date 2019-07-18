module Logic

type Atomic =
  | True
  | False
  | Var of string

type Not<'a> =
  | Not of 'a

type Literal =
  | Atomic of Atomic
  | LNot of Not<Atomic>

type SemiLiteral =
  | Literal of Literal
  | SLNot of Not<SemiLiteral>

type Term =
  | SemiLiteral of SemiLiteral
  | And of seq<Term>
  | Or of seq<Term>

type AndTerm =
  | Literal of Literal
  | And of seq<Literal>

type DNF = 
  | AndTerm of AndTerm
  | Or of seq<AndTerm>

type OrTerm =
  | Literal of Literal
  | Or of seq<Literal>

type CNF = 
  | OrTerm of OrTerm
  | And of seq<OrTerm>
