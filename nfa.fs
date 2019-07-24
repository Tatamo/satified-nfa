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

let p (index:int) (state:State) = Var("p" + index.ToString() + "_" + (match state with State(i) -> i.ToString()))
let x (index:int) = Var("x" + index.ToString())

let char2bool c = (c <> '0')
let input2Props (input:string) =
  input
  |> Seq.toList
  |> Seq.indexed
  |> Seq.map (fun (index, c) ->
    if char2bool c then
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

let rules2PropsTrue' i =
  rules
  |> Seq.map (fun rule ->
    match rule with
    | (from, input, next) ->
    // p(state, i) AND (~)x(input, i+1) -> OR{ p(s, i+1) : for all s∈states, Rule(s, input, next) ∈ rules }
    // Not p(..) OR Not x(..) OR (OR{p(s, i+1) : for all s∈states, Rule(s, input, next) ∈ rules })
      let p' = p i from in
      let x' = x (i + 1) in
      let x'' = if not (char2bool input) then Atomic x' else LNot (Not x') in
      Seq.concat([
        seq [LNot (Not p'); x''];
        states
        |> Seq.filter (fun s -> s = next)
        |> Seq.map (fun s -> Atomic (p (i+1) s))
      ]) |> OrForm.Or
  )

// TODO: p(1, i) AND (x(input, i+1)=0) -> AND {NOT p(s,i+1) for all s∈states} のときの制約がない
let rules2PropsFalse' i =
  Seq.collect (fun rule ->
    match rule with
    | (from, input, next) ->
      // p(from, i) AND (~)x(input, i+1) -> AND{ Not p(s, i+1) : for all s∈states, Rule(s, input, next) ∉ rules }
      // AND { Not p(..) OR Not x(..) OR Not p(s, i+1) : for all s∈states, Rule(s, input, next) ∉ rules }
      let p' = p i from in
      let x' = x (i + 1) in
      let x'' = if not (char2bool input) then Atomic x' else LNot (Not x') in
      states
      |> Seq.filter (fun s -> s <> next)
      |> Seq.map (fun s ->
        OrForm.Or [LNot (Not p'); x''; LNot (Not (p (i + 1) s))]
       )) rules

let rules2PropsTrue n = seq {0 .. n-1} |> Seq.collect rules2PropsTrue'
let rules2PropsFalse n = seq {0 .. n-1} |> Seq.collect rules2PropsFalse'

let terms (input:string) =
  let n = input.Length in
  [input2Props input; pInitials start; pTerminals accepts n; rules2PropsTrue n; rules2PropsFalse n]
  |> Seq.concat
  |> CNF.And
