// Learn more about F# at http://fsharp.org

open System
open FSharpFunctionSandbox
open Units.SI.UnitNames


[<EntryPoint>]
let main argv =
    let temperature = FSharpFunctionSandbox.FSharpFunctionSandbox.LengthConversion.cmToFoot(100m<centimeter>)
    printfn "%A" temperature
    0 // return an integer exit code
