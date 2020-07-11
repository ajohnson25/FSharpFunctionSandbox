// Learn more about F# at http://fsharp.org

open System
open FSharpFunctionSandbox


[<EntryPoint>]
let main argv =
    printfn "%A" FSharpFunctionSandbox.Say.want
    //let temperature = FSharpFunctionSandbox.FSharpFunctionSandbox.MeasureConversion.cmToFoot(100m<Units.centimeter>)
    //printfn "%A" temperature
    0 // return an integer exit code
