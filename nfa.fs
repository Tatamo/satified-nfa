module NFA
open Logic

type Alphabet = char
type Alphabets = Set<Alphabet>
type State = State of int
type States = Set<State>
type Rule = State * Alphabet * State
type Rules = Set<Rule>

type NFA = Alphabets * States * Rules * State * States

let inputString = "0110"
let inputLength = inputString.Length

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

//let p (state:State) (index:int) = Var("p" + index.ToString() + (match state with State(i) -> i.ToString()))
//let x (index:int) = Var("x" + index.ToString())
let indexState state =
  match state with State i -> i
let p (state:State) (index:int) = Var((inputLength + index * states.Count + indexState state).ToString())
let x (index:int) = Var((index - 1).ToString())

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
      OrForm.Or [Atomic (p state 0)] else
      OrForm.Or [LNot (Not (p state 0))])

let pTerminals accepts inputLength =
  states
  |> Seq.map (fun state ->
    if Set.contains state accepts then
      OrForm.Or [Atomic (p state inputLength)] else
      OrForm.Or [LNot (Not (p state inputLength))])

let rules2PropsTrue' i =
  rules
  |> Seq.map (fun rule ->
    match rule with
    | (from, input, next) ->
    // p(state, i) AND x(input, i+1) -> OR{ p(s, i+1) : for all s∈states, Rule(s, input, next) ∈ rules }
    // Not p(..) OR Not x(..) OR (OR{p(s, i+1) : for all s∈states, Rule(s, input, next) ∈ rules })
      let p' = p from i in
      let x' = x (i + 1) in
      let x'' = if char2bool input then Atomic x' else LNot (Not x') in
      Seq.concat([
        seq [LNot (Not p'); x''];
        states
        |> Seq.filter (fun s -> s = next)
        |> Seq.map (fun s -> Atomic (p s (i+1)))
      ]) |> OrForm.Or
  )

let rules2PropsFalse' i =
  Seq.collect (fun rule ->
    match rule with
    | (from, input, next) ->
      // p(from, i) AND x(input, i+1) -> AND{ Not p(s, i+1) : for all s∈states, Rule(s, input, next) ∉ rules }
      // AND { Not p(..) OR Not x(..) OR Not p(s, i+1) : for all s∈states, Rule(s, input, next) ∉ rules }
      let p' = p from i in
      let x' = x (i + 1) in
      let x'' = if char2bool input then Atomic x' else LNot (Not x') in
      states
      |> Seq.filter (fun s -> s <> next)
      |> Seq.map (fun s ->
        OrForm.Or [LNot (Not p'); x''; LNot (Not (p s (i + 1)))]
       )) rules

let rules2PropsTrue n = seq {0 .. n-1} |> Seq.collect rules2PropsTrue'
let rules2PropsFalse n = seq {0 .. n-1} |> Seq.collect rules2PropsFalse'

let terms =
  [input2Props inputString; pInitials start; pTerminals accepts inputLength; rules2PropsTrue inputLength; rules2PropsFalse inputLength]
  |> Seq.concat
  |> CNF.And
