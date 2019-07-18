module NFA
open Logic

type Alphabet = string
type Alphabets = Set<Alphabet>
type State = State of int
type States = Set<State>
type Rule = State * Alphabet * State
type Rules = Set<Rule>

type NFA = Alphabets * States * Rules * State * States

let alphabets = Set.ofList ["0"; "1"]
let q0 = State(0)
let q1 = State(1)
let q2 = State(2)
let states = Set.ofList [q0; q1; q2]
let start = q0
let accepts = Set.ofList [q2]
let rules = Set.ofList [
  Rule(q0, "0", q0);
  Rule(q0, "1", q1);
  Rule(q1, "1", q2);
  Rule(q2, "0", q2)
]
let nfa = (alphabets, states, rules, start, accepts)

//let code nfa:NFA n:int =
