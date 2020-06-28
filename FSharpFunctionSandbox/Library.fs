namespace FSharpFunctionSandbox

open Microsoft.FSharp.Data.UnitSystems.SI
open Units

module FSharpFunctionSandbox =

    module Say =
        let hello name =
            printfn "Hello %s" name

    module MeasureConversion =
        //Metric to metric weight
        let gramsPerKilogram : decimal<SIUF.Name.gram/UnitNames.kilogram> = 1000m<SIUF.Name.gram/UnitNames.kilogram>
        let milligramsPerGram : decimal<SIUF.Name.milligram/SIUF.Name.gram> = 1000m<SIUF.Name.milligram/SIUF.Name.gram>
        let cmPerMeter: decimal<SIUF.Name.centimeter/UnitNames.meter> = 100m<SIUF.Name.centimeter/UnitNames.meter>

        //Metric to Imperial length, this conversion is lossy
        let cmPerInch : decimal<SIUF.Name.centimeter/Imperial.Name.inch> = 2.54m<SIUF.Name.centimeter/Imperial.Name.inch>
        let cmPerThou : decimal<Imperial.Name.thou/SIUF.Name.centimeter> = 394m<Imperial.Name.thou/SIUF.Name.centimeter>
        let cmPerFoot : decimal<SIUF.Name.centimeter/Imperial.Name.foot> = 30.48m<SIUF.Name.centimeter/Imperial.Name.foot>

        let thousPerInch : decimal<Imperial.Name.thou/Imperial.Name.inch> = 1000m<Imperial.Name.thou/Imperial.Name.inch>
        let inchesPerFoot : decimal<Imperial.Name.inch/Imperial.Name.foot> = 12m<Imperial.Name.inch/Imperial.Name.foot>

        
        let mgToG (mg : decimal<SIUF.Name.milligram>) = mg / milligramsPerGram
        let gToKg (g: decimal<SIUF.Name.gram>) = g  / gramsPerKilogram
        let mgToKg = mgToG >> gToKg

        let kgToG (kg : decimal<UnitNames.kilogram>) = kg * gramsPerKilogram
        let gToMg (g : decimal<SIUF.Name.gram>) = g * milligramsPerGram
        let kgToMg = kgToG >> gToMg

        let cmToIn (cm: decimal<SIUF.Name.centimeter>) = cm / cmPerInch
        let cmToThou (cm: decimal<SIUF.Name.centimeter>) = cm * cmPerThou
        let cmToFoot (cm: decimal<SIUF.Name.centimeter>) = cm / cmPerFoot

        let inchToThou (inch: decimal<Imperial.Name.inch>) = inch * thousPerInch
        let footToInch (foot: decimal<Imperial.Name.foot>) = foot * inchesPerFoot

        let thouToInch (thou: decimal<Imperial.Name.thou>) = thou / thousPerInch
        let inchToFoot (inch: decimal<Imperial.Name.inch>) = inch / inchesPerFoot
        let thouToFoot = thouToInch >> inchToFoot

    module TemperatureConversion =
        let FahrenheitToCelsius fahrenheit = (fahrenheit - 32m) * (5m/ 9m) //0.55 repeating
        let CelsiusToFahrenheit celsius = (celsius * (9m/5m)) + 32m

    module FizzBuzz =
        let RunFB x =
            match x with
            | i when i % 15 = 0 -> printfn "%i:FizzBuzz" i
            | i when i % 3 = 0 -> printfn "%i:Fizz" i
            | i when i % 5 = 0 -> printfn "%i:Buzz" i
            | i -> printfn "%i" i
        let oneToOneHundred () = [1..100] |> List.iter RunFB