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

        //Metric to metric length
        let mmPerCm: decimal<SIUF.Name.millimeter/SIUF.Name.centimeter> = 10m<SIUF.Name.millimeter/SIUF.Name.centimeter>
        let cmPerMeter: decimal<SIUF.Name.centimeter/UnitNames.meter> = 100m<SIUF.Name.centimeter/UnitNames.meter>

        //Metric to Imperial length, this conversion is lossy
        let cmPerInch : decimal<SIUF.Name.centimeter/Imperial.Name.inch> = 2.54m<SIUF.Name.centimeter/Imperial.Name.inch>
        let cmPerThou : decimal<Imperial.Name.thou/SIUF.Name.centimeter> = 394m<Imperial.Name.thou/SIUF.Name.centimeter>
        let cmPerFoot : decimal<SIUF.Name.centimeter/Imperial.Name.foot> = 30.48m<SIUF.Name.centimeter/Imperial.Name.foot>

        //Imperial Length Up
        let thousPerInch : decimal<Imperial.Name.thou/Imperial.Name.inch> = 1000m<Imperial.Name.thou/Imperial.Name.inch>
        let inchesPerFoot : decimal<Imperial.Name.inch/Imperial.Name.foot> = 12m<Imperial.Name.inch/Imperial.Name.foot>
        let feetPerYard : decimal<Imperial.Name.foot/Imperial.Name.yard> = 3m<Imperial.Name.foot/Imperial.Name.yard>

        //Metric Length Up
        let mmToCm (mm: decimal<SIUF.Name.millimeter>) = mm / mmPerCm
        let cmToMeter (cm: decimal<SIUF.Name.centimeter>) = cm / cmPerMeter
        let mmToMeter = mmToCm >> cmToMeter

        //Metric Length Down
        let meterToCm (meter: decimal<UnitNames.meter>) = meter * cmPerMeter
        let cmToMm (centimeter: decimal<SIUF.Name.centimeter>) = centimeter * mmPerCm
        let meterToMm = meterToCm >> cmToMm
        
        let mgToG (mg : decimal<SIUF.Name.milligram>) = mg / milligramsPerGram
        let gToKg (g: decimal<SIUF.Name.gram>) = g  / gramsPerKilogram
        let mgToKg = mgToG >> gToKg

        let kgToG (kg : decimal<UnitNames.kilogram>) = kg * gramsPerKilogram
        let gToMg (g : decimal<SIUF.Name.gram>) = g * milligramsPerGram
        let kgToMg = kgToG >> gToMg

        let cmToIn (cm: decimal<SIUF.Name.centimeter>) = cm / cmPerInch
        let cmToThou (cm: decimal<SIUF.Name.centimeter>) = cm * cmPerThou
        let cmToFoot (cm: decimal<SIUF.Name.centimeter>) = cm / cmPerFoot

        let yardToFeet (yard: decimal<Imperial.Name.yard>) = yard * feetPerYard
        let footToInch (foot: decimal<Imperial.Name.foot>) = foot * inchesPerFoot
        let inchToThou (inch: decimal<Imperial.Name.inch>) = inch * thousPerInch
        let yardToInch = yardToFeet >> footToInch
        let yardToThou = yardToInch >> inchToThou
        
        //lossy conversions because of irrational division
        let footToYard (foot: decimal<Imperial.Name.foot>) = foot / feetPerYard
        let thouToInch (thou: decimal<Imperial.Name.thou>) = thou / thousPerInch
        let inchToFoot (inch: decimal<Imperial.Name.inch>) = inch / inchesPerFoot
        let thouToFoot = thouToInch >> inchToFoot
        let thouToYard = thouToFoot >> footToYard

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