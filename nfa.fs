module NFA
open Logic

type Alphabet = char
type Alphabets = Set<Alphabet>
type State = State of int
type States = Set<State>
type Rule = State * Alphabet * State
type Rules = Set<Rule>

type NFA = Alphabets * States * Rules * State * States

let alphabets = Set.ofList ['0'; '1']
let q0 = State(0)
let q1 = State(1)
let q2 = State(2)
let states = Set.ofList [q0; q1; q2]
let start = q0
let accepts = Set.ofList [q2]
let rules = Set.ofList [
  Rule(q0, '0', q0);
  Rule(q0, '1', q1);
  Rule(q1, '1', q2);
  Rule(q2, '0', q2)
]
let nfa = (alphabets, states, rules, start, accepts)

type MergedRules = Set<State * Alphabet * Set<State>>
let mergeRules (rules:Rules) (states:States) =
 [for state in states do
  for alphabet in alphabets ->
  let nextStates =  (
    rules
    |> Seq.filter (fun (from, alphabet', _) -> from = state && alphabet' = alphabet)
    |> Seq.map (fun (_, _, next) -> next)) |> Set.ofSeq in
  (state, alphabet, nextStates)
 ]

let p (index:int) (state:State) = Var("p" + index.ToString() + "_" + (match state with State(i) -> i.ToString()))
let x (index:int) = Var("x" + index.ToString())

let inputChar2Prop c =
  if c = '1' then Some true else
  if c = '0' then Some false else
  None

let char2bool c = (c <> '0')
let input2Props (input:string) =
  input
  |> Seq.toList
  |> Seq.choose inputChar2Prop
  |> Seq.indexed
  |> Seq.map (fun (index, b) ->
    if b then
      OrForm.Literal(Atomic (x (index + 1))) else
      OrForm.Literal(LNot (Not (x (index + 1))))
  )
// for all states that is not start state, px0=false
let pInitials start =
  states
  |> Seq.map (fun state ->
    if state = start then
      OrForm.Or [Atomic (p 0 state)] else
      OrForm.Or [LNot (Not (p 0 state))])

let pTerminals accepts inputLength =
  states
  |> Seq.map (fun state ->
    if Set.contains state accepts then
      OrForm.Or [Atomic (p inputLength state)] else
      OrForm.Or [LNot (Not (p inputLength state))])

let rules2PropsTrue' mergedRules i =
 [for (from, alphabet, nexts) in mergedRules do
  // p(i, from) AND  x(i+1)=input -> OR { p(i+1, next) : for all next ∈ nexts}
  // NOT p.. OR NOT x.. OR ( OR{ p(i+1, next) : for all next ∈ nexts} )
  let p' = LNot (Not (p i from)) in
  let x' =
    if not (char2bool alphabet) // revert literal (for NOT x...)
    then Atomic (x (i + 1)) // x(i+1)=1
    else LNot (Not (x (i + 1))) in // x(i+1)=0
  if Set.isEmpty nexts then () else
  let nextLiterals = nexts |> Seq.map (fun next -> Atomic (p (i+1) next)) in
  yield Seq.concat([
    seq [p'; x'];
    nextLiterals
  ]) |> OrForm.Or
 ] |> seq

let rules2PropsFalse' mergedRules i =
 [for (from, alphabet, nexts) in mergedRules do
  // p(i, from) AND x(i+1)=input -> AND{ NOT p(i+1, next) : for all next ∈ (states \ nexts) }
  // AND { NOT p.. OR NOT x.. OR NOT p(i+1, next) : for all next ∈ (states \ nexts)} )
  let p' = LNot (Not (p i from)) in
  let x' =
    if not (char2bool alphabet) // revert literal (for NOT x...)
    then Atomic (x (i + 1)) // x(i+1)=1
    else LNot (Not (x (i + 1))) in // x(i+1)=0
  let notNexts = Set.difference states nexts
  if Set.isEmpty notNexts then () else
  let nextLiterals = notNexts |> Seq.map (fun next -> LNot (Not (p (i+1) next))) in
  yield seq { for n in nextLiterals -> OrForm.Or [p'; x'; n] }
 ] |> Seq.concat

let rules2Props n =
  let mergedRules = mergeRules rules states
  [
    seq {0 .. n-1} |> Seq.collect (rules2PropsTrue' mergedRules);
    seq {0 .. n-1} |> Seq.collect (rules2PropsFalse' mergedRules)
  ] |> Seq.concat

// for each input index 0<=i<=n, at least one of p(?, i) is true
let stateConstraints n =
  seq {0 .. n}
  |> Seq.map (fun i ->
    OrForm.Or( states |> Seq.map (fun state -> Atomic (p i state)))
  )

let terms (input:string) =
  let n = input.Length in
//  [input2Props input; pInitials start; pTerminals accepts n; rules2PropsTrue n; rules2PropsFalse n]
  [input2Props input; pInitials start; pTerminals accepts n; rules2Props n; stateConstraints n]
  |> Seq.concat
  |> CNF.And
