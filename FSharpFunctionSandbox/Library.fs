namespace FSharpFunctions

open UnitsUOM.Imperial.UnitNames
open UnitsUOM.SI.UnitNames


module FSharpFunctions =

    module Dilution =
        let diluteGetTotalSG startingSg endingSg startingVolume endingVolume = ((startingVolume * startingSg) + (endingVolume * endingSg))/startingVolume+endingVolume
        let diluteFindBegVolume endingVolume endingSg startingSg = (endingVolume * endingSg) / startingSg
        let diluteFindBegSg endingVolume endingSg startingVolume = (endingVolume * endingSg) / startingVolume
        let diluteFindEndVolume startingVolume startingSg endingSg = (startingVolume * startingSg) /endingSg
        let diluteFindEndSg startingVolume beginningSg endingVolume = (startingVolume * beginningSg) / endingVolume

    module Say =
        let hello name =
            printfn "Hello %s" name


    module LengthConversion =
        //Metric to metric length
        let mmPerCm: decimal<millimeter/centimeter> = 10m<millimeter/centimeter>
        let cmPerMeter: decimal<centimeter/meter> = 100m<centimeter/meter>

        //Metric to Imperial length, this conversion is lossy so be careful on 1+ convs
        let metersPerYard: decimal<meter/yard> = 0.9144m<meter/yard> //International yard

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
        let mmToMeter x = mmToCm x |> cmToMeter

        //Imperial conversion down
        let yardToFoot (yard: decimal<yard>) = yard * feetPerYard
        let footToInch (foot: decimal<foot>) = foot * inchesPerFoot
        let inchToThou (inch: decimal<inch>) = inch * thousPerInch
        let yardToInch x = yardToFoot x |> footToInch
        let yardToThou x = yardToInch x |> inchToThou

        //Metric Length Down
        let meterToCm (meter: decimal<meter>) = meter * cmPerMeter
        let cmToMm (centimeter: decimal<centimeter>) = centimeter * mmPerCm
        let meterToMm x = meterToCm x |> cmToMm

        //lossy conversions because of irrational division
        //Imperial conversion up
        let thouToInch (thou: decimal<thou>) = thou / thousPerInch
        let inchToFoot (inch: decimal<inch>) = inch / inchesPerFoot
        let footToYard (foot: decimal<foot>) = foot / feetPerYard
        let thouToFoot x = thouToInch x |> inchToFoot
        let inchToYard x = inchToFoot x |> footToYard
        let thouToYard x = thouToFoot x |> footToYard

        let meterToYard (m: decimal<meter>) = m / metersPerYard
        let cmToYard x = cmToMeter x |> meterToYard
        let cmToFoot x = cmToYard x |> yardToFoot
        let cmToInch x = cmToFoot x |> footToInch
        let cmToThou x = cmToInch x |> inchToThou

        let yardToMeter (yd: decimal<yard>) = yd * metersPerYard

        let thouToMeter x = thouToYard x |> yardToMeter
        let inchToMeter x = inchToYard x |> yardToMeter
        let footToMeter x = footToYard x |> yardToMeter

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