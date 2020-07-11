namespace FSharpFunctionSandbox

open Units.Imperial.Name
open Units.SI.Name

module FSharpFunctionSandbox =

    module Say =
        let hello name =
            printfn "Hello %s" name

        let want = nameof nameof

    module MeasureConversion =
        //Metric to metric weight
        let gramsPerKilogram : decimal<gram/kilogram> = 1000m<gram/kilogram>
        let milligramsPerGram : decimal<milligram/gram> = 1000m<milligram/gram>

        //Metric to metric length
        let mmPerCm: decimal<millimeter/centimeter> = 10m<millimeter/centimeter>
        let cmPerMeter: decimal<centimeter/meter> = 100m<centimeter/meter>

        //Metric to Imperial length, this conversion is lossy
        let cmPerInch : decimal<centimeter/inch> = 2.54m<centimeter/inch>
        let cmPerThou : decimal<thou/centimeter> = 394m<thou/centimeter>
        let cmPerFoot : decimal<centimeter/foot> = 30.48m<centimeter/foot>

        //Imperial Length Up
        let thousPerInch : decimal<thou/inch> = 1000m<thou/inch>
        let inchesPerFoot : decimal<inch/foot> = 12m<inch/foot>
        let feetPerYard : decimal<foot/yard> = 3m<foot/yard>

        //Metric Length Up
        let mmToCm (mm: decimal<millimeter>) = mm / mmPerCm
        let cmToMeter (cm: decimal<centimeter>) = cm / cmPerMeter
        let mmToMeter = mmToCm >> cmToMeter

        //Metric Length Down
        let meterToCm (meter: decimal<meter>) = meter * cmPerMeter
        let cmToMm (centimeter: decimal<centimeter>) = centimeter * mmPerCm
        let meterToMm = meterToCm >> cmToMm
        
        //Metric Weight convert up
        let mgToG (mg : decimal<milligram>) = mg / milligramsPerGram
        let gToKg (g: decimal<gram>) = g  / gramsPerKilogram
        let mgToKg = mgToG >> gToKg

        //Metric Weight convert down
        let kgToG (kg : decimal<kilogram>) = kg * gramsPerKilogram
        let gToMg (g : decimal<gram>) = g * milligramsPerGram
        let kgToMg = kgToG >> gToMg

        let cmToIn (cm: decimal<centimeter>) = cm / cmPerInch
        let cmToThou (cm: decimal<centimeter>) = cm * cmPerThou
        let cmToFoot (cm: decimal<centimeter>) = cm / cmPerFoot

        //Imperial conversion down
        let yardToFeet (yard: decimal<yard>) = yard * feetPerYard
        let footToInch (foot: decimal<foot>) = foot * inchesPerFoot
        let inchToThou (inch: decimal<inch>) = inch * thousPerInch
        let yardToInch = yardToFeet >> footToInch
        let yardToThou = yardToInch >> inchToThou
        
        //lossy conversions because of irrational division
        //Imperial conversion up
        let footToYard (foot: decimal<foot>) = foot / feetPerYard
        let thouToInch (thou: decimal<thou>) = thou / thousPerInch
        let inchToFoot (inch: decimal<inch>) = inch / inchesPerFoot
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