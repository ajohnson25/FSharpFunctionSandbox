// Learn more about F# at http://fsharp.org

open System
open FSharpFunctionSandbox
open Units.SI.UnitNames


[<EntryPoint>]
let main argv =
    let temperature = FSharpFunctionSandbox.FSharpFunctionSandbox.VolumeConversion.litersToGallonsUs(3.8m<liter>)
    printfn "%A" temperature
    0 // return an integer exit code
