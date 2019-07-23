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
let inline input f = System.Console.ReadLine() |> f

[<EntryPoint>]
let main argv =
    // printfn "%A" (gterm |> convertDeMorgan |> literalize |> formatLT)
    // printfn "%A" (gterm |> convertDeMorgan |> literalize |> mergeDuplicateAndOr |> formatLT)
    printfn "%s" (terms (input string) |> formatCNF)
    0 // return an integer exit code
