// Learn more about F# at http://fsharp.org

open System
open FSharpFunctions
open UnitsUOM.SI.UnitNames


[<EntryPoint>]
let main argv =
    let temperature = FSharpFunctions.FSharpFunctions.VolumeConversion.convertVolume(1.00000033435325741461344m,"milliliters","minimsUk")
    printfn "%A" temperature
    0 // return an integer exit code
