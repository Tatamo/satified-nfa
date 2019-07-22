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

type MultiNotAtomic =
  | DoubleNot of Not<Not<Atomic>>
  | MultiNot of Not<MultiNotAtomic>

type SemiLiteral =
  | Literal of Literal
  | MultiNotSemiLiteral of MultiNotAtomic

let rec literalize semiLiteral =
  match semiLiteral with
  | Literal(a) -> a
  | MultiNotSemiLiteral(mn) ->
    match mn with
    | DoubleNot(Not (Not x)) -> Atomic x
    | MultiNot(Not x) ->
      match x with
      | DoubleNot(Not (Not x)) -> LNot (Not x)
      | MultiNot(Not x) -> literalize(MultiNotSemiLiteral x)


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
