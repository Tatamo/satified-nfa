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
let n = inputString.Length

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

let p (state:State) (index:int) = Var("p" + (match state with State(i) -> i.ToString()) + "-" + index.ToString() )
let x (input:char) (index:int) = Var("x" + string input + "-" + index.ToString())

let input2Props (input:string) =
  input
  |> Seq.toList
  |> Seq.indexed
  |> Seq.map (fun (index, c) ->
    let char2bool c = (c <> '0') in
    if char2bool c then
      OrForm.Literal(Atomic (x c index)) else
      OrForm.Literal(LNot (Not (x c index)))
  )
// for all states that is not start state, px0=false
let pInitials start =
  states
  |> Seq.map (fun state ->
    if state = start then
      Atomic (p state 0) else
      LNot (Not (p state 0)))
  |> OrForm.Or
let rules2Props' n =
  Seq.collect (fun rule ->
    match rule with
    | (state, input, next) ->
      // p(state, 0) AND x(input, 1) -> OR{ Not p(s, 1) : for all s∈states, Rule(state, input, next) ∉ rules }
      // AND { Not p(..) OR Not x(..) OR Not p(s, 1) : for all s∈states, Rule(state, input, next) ∉ rules }
      let p' = p state n in
      let x' = x input (n + 1) in
      states
      |> Seq.filter (fun state -> state <> next)
      |> Seq.map (fun state ->
        OrForm.Or [LNot (Not p'); LNot (Not x'); LNot (Not (p state (n + 1)))]
       )) rules
let rules2Props n = seq {0 .. n-1} |> Seq.collect rules2Props'

let terms =
  [input2Props inputString;seq [pInitials start]; rules2Props n]
  |> Seq.concat
  |> CNF.And

//let code nfa:NFA n:int =
