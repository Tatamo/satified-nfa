// Learn more about F# at http://fsharp.org

open System
open Logic
open NFA
let gterm = GNot (GAnd [
  GNot (GAtomic True);
  GOr[
    GAtomic True;
    GAnd [GAtomic True; GAtomic False];
    GOr [GAtomic False; GNot (GAtomic False)];
    GAtomic False
  ]
])

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    printfn "%A" (gterm |> convertDeMorgan |> literalize |> formatLT)
    printfn "%A" (gterm |> convertDeMorgan |> literalize |> mergeDuplicateAndOr |> formatLT)
    printfn "%A" (terms |> formatCNF)
    0 // return an integer exit code
