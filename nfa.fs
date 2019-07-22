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

let p (state:State) (index:int) = Var("p" + state.ToString() + "-" + index.ToString() )
let x (input:char) (index:int) = Var("x" + string input + "-" + index.ToString())

// for all states that is not start state, px0=false
let pInitials =
  states
  |> Seq.map (fun state ->
    if states.Contains state then
      Atomic (p state 0) else
      LNot (Not (p state 0)))
let rules1 =
  rules
  |> Seq.map (fun rule ->
    match rule with
    | (state, input, next) ->
      // p(state, 0) AND x(1,input) -> OR{ Not p(s, 1) : for all s∈states, Rule(state, input, next) ∉ rules }
      // AND { Not p(..) OR Not x(..) OR Not p(s, 1) : for all s∈states, Rule(state, input, next) ∉ rules }
      () // [WIP]
  )

let terms = []

//let code nfa:NFA n:int =
