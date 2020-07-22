namespace FSharpFunctionSandbox

open Units.Imperial.UnitNames
open Units.SI.UnitNames


module FSharpFunctionSandbox =

    module Say =
        let hello name =
            printfn "Hello %s" name

        let want = nameof nameof

    module WeightConversion =
        //Metric to metric weight
        let gramsPerKilogram : decimal<gram/kilogram> = 1000m<gram/kilogram>
        let milligramsPerGram : decimal<milligram/gram> = 1000m<milligram/gram>
        
        let grainsPerPound: decimal<grain/pound> = 7000m<grain/pound>
        let drachmPerPound: decimal<drachm/pound> = 256m<drachm/pound>
        let drachmPerOunce: decimal<drachm/ounce> = 16m<drachm/ounce>
        let ouncesPerPound: decimal<ounce/pound> = 16m<ounce/pound>
        let poundsPerStone: decimal<pound/stone> = 14m<pound/stone>
        let quarterPerStone: decimal<quarter_wt/stone> = 2m<quarter_wt/stone>
        let poundsPerQuarter: decimal<pound/quarter_wt> = 28m<pound/quarter_wt>
        let poundsPerHundredweight: decimal<pound/hundredweight> = 112m<pound/hundredweight>
        let stonesPerHundredweight: decimal<stone/hundredweight> = 8m<stone/hundredweight>
        let poundsPerTon: decimal<pound/ton> = 2240m<pound/ton>
        let hundredweightPerTon: decimal<hundredweight/ton> = 20m<hundredweight/ton>


        //Metric Weight convert up
        let mgToG (mg : decimal<milligram>) = mg / milligramsPerGram
        let gToKg (g: decimal<gram>) = g  / gramsPerKilogram
        let mgToKg = mgToG >> gToKg

        //Metric Weight convert down
        let kgToG (kg : decimal<kilogram>) = kg * gramsPerKilogram
        let gToMg (g : decimal<gram>) = g * milligramsPerGram
        let kgToMg = kgToG >> gToMg

    module VolumeConversion =
        //based on international yard
        let litersPerUKGallon:decimal<liter/gallon_uk> = 4.5609m<liter/gallon_uk>
        let mlPerFlOzUS:decimal<milliliter/fluidOunce_us> = 29.5735295625m<milliliter/fluidOunce_us>
        let usGallonsPerUkGallon:decimal<gallon_us/gallon_uk> = 1.201m<gallon_us/gallon_uk>

        let usGallonToUkGallon (usGallon: decimal<gallon_us>) = usGallon / usGallonsPerUkGallon
        let mlPerUSGallon = mlPerFlOzUS * 128m<fluidOunce_us>

    module LengthConversion =

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