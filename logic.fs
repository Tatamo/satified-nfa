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

type GeneralTerm =
  | Atomic of Atomic
  | Not of GeneralTerm
  | And of seq<GeneralTerm>
  | Or of seq<GeneralTerm>

type AndForm =
  | Literal of Literal
  | And of seq<Literal>

type DNF = 
  | AndForm of AndForm
  | Or of seq<AndForm>

type OrForm =
  | Literal of Literal
  | Or of seq<Literal>

type CNF = 
  | OrForm of OrForm
  | And of seq<OrForm>


let rec convertDeMorgan term =
  match term with
  | Not x ->
    match x with
    | GeneralTerm.And y -> GeneralTerm.Or (Seq.map (Not >> convertDeMorgan) y)
    | GeneralTerm.Or y -> GeneralTerm.And (Seq.map (Not >> convertDeMorgan) y)
    | _ -> Not (convertDeMorgan x)
  | x -> x

