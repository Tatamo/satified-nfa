﻿// Learn more about F# at http://fsharp.org

open System
open Logic

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#! %A" (Not(True))
    0 // return an integer exit code