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
  | SLAtomic of Atomic
  | SLNot of Not<SemiLiteral>

type GeneralTerm =
  | GAtomic of Atomic
  | GNot of GeneralTerm
  | GAnd of seq<GeneralTerm>
  | GOr of seq<GeneralTerm>

type SemiLiteralTerm =
  | SLTLiteral of SemiLiteral
  | SLTAnd of seq<SemiLiteralTerm>
  | SLTOr of seq<SemiLiteralTerm>

type LiteralTerm =
  | LTLiteral of Literal
  | LTAnd of seq<LiteralTerm>
  | LTOr of seq<LiteralTerm>

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
  | GAtomic x -> SLTLiteral (SLAtomic x)
  | GNot x ->
    match x with
    | GAtomic y -> SLTLiteral (SLNot (Not (SLAtomic y)))
    | GNot y -> convertDeMorgan y // remove NotNot
    | GAnd y -> SLTOr (Seq.map (GNot >> convertDeMorgan) y)
    | GOr y -> SLTAnd (Seq.map (GNot >> convertDeMorgan) y)
  | GAnd x -> SLTAnd (Seq.map convertDeMorgan x)
  | GOr x -> SLTOr (Seq.map convertDeMorgan x)

let rec literalize semilLiteralTerm =
  match semilLiteralTerm with
  | SLTLiteral x ->
    match x with
    | SLAtomic y -> LTLiteral (Atomic y)
    | SLNot (Not y) ->
    match y with
      | SLAtomic z -> LTLiteral (LNot (Not z))
      | SLNot (Not z) -> literalize (SLTLiteral z)
  | SLTAnd x -> LTAnd(Seq.map literalize x)
  | SLTOr x -> LTOr(Seq.map literalize x)

let rec formatLiteral literal =
  match literal with
  | Atomic x ->
    match x with
    | True -> "True"
    | False -> "False"
    | Var(name) -> name
  | LNot (Not x) -> "NOT " + (formatLiteral (Atomic x))
let rec formatLT literalTerm =
  match literalTerm with
  | LTLiteral x -> formatLiteral x
  | LTAnd x -> "(" + (String.concat ") AND (" (Seq.map formatLT x)) + ")"
  | LTOr x -> "(" + (String.concat ") OR (" (Seq.map formatLT x)) + ")"
