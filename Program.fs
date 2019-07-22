// Learn more about F# at http://fsharp.org

open System
open Logic
let gterm = GNot (GNot (GNot (GAnd [GNot (GAtomic True); GOr[GAtomic True; GAnd [GAtomic True; GAtomic False]; GAtomic False]])))

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    printfn "%A" (literalize (convertDeMorgan gterm))
    0 // return an integer exit code
