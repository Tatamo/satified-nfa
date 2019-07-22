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
    | Var name -> name
  | LNot (Not x) -> "NOT " + (formatLiteral (Atomic x))
let rec formatLT literalTerm =
  match literalTerm with
  | LTLiteral x -> formatLiteral x
  | LTAnd x -> "(" + (String.concat ") AND (" (Seq.map formatLT x)) + ")"
  | LTOr x -> "(" + (String.concat ") OR (" (Seq.map formatLT x)) + ")"

let rec mergeDuplicateAndOr literalTerm =
  match literalTerm with
  | LTLiteral _ -> literalTerm
  | LTAnd x ->
    let isAnd lt = match lt with LTAnd _ -> true | _ -> false in
    LTAnd (
      Seq.concat [
        Seq.filter (isAnd >> not) x;
        x
        |> Seq.filter isAnd
        |> Seq.collect (fun e ->  match e with LTAnd x -> x | _ -> seq [])
      ] |> Seq.map mergeDuplicateAndOr
    )
  | LTOr x ->
    let isOr lt = match lt with LTOr _ -> true | _ -> false in
    LTOr (
      Seq.concat [
        Seq.filter (isOr >> not) x;
        x
        |> Seq.filter isOr
        |> Seq.collect (fun e ->  match e with LTOr x -> x | _ -> seq [])
      ] |> Seq.map mergeDuplicateAndOr
    )

(*
type LiteralTerm' =
  | LTLiteral' of Literal
  | LTAnd' of LiteralTerm' * LiteralTerm'
  | LTOr' of LiteralTerm' * LiteralTerm'

let disassembleAnd term =
  match term with
  | LTAnd x ->
    match (Seq.toList x) with
    | fst :: snd :: tail -> fst
    | [e] -> e
    | [] ->
    x |> Seq.sortBy (fun e ->
    match e with
    | LTLiteral _ -> 0
    | LTOr _ -> 1
    | _ -> 2)
  | _ -> term
*)

(*
let rec convert2ary literalTerm =
  match literalTerm with
  | LTLiteral x -> LTLiteral' x
  | LTAnd x ->
    
    *)


// A AND (B OR C)
// (A AND B) OR (A AND C)
// (X OR Y) AND (X OR Z)
// ((X OR Y) AND X) OR ((X OR Y) AND Z)

(*
let rec convert2CNF literalTerm =
  match literalTerm with
  | LTLiteral x -> AndForm (AndForm.Literal x)
  | LTAnd x ->
    AndForm (AndForm.And(x |> Seq.map (fun x->
      match x with
      | LTLiteral y -> y
      | 
    )))
*)

(*
let nt = LNot (Not True)
let nnt = DoubleNot (Not (Not True))
let nnnt = Not nnt
let nL = LNot (Not True)
let dn = DoubleNot (Not (Not True))
let tn = MultiNot (Not dn)
let sl = MultiNotSemiLiteral tn
let sl' = MultiNotSemiLiteral (MultiNot (Not (DoubleNot (Not (Not True)))))
*)
