namespace FSharpFunctionSandbox

open Units.Imperial.UnitNames
open Units.SI.UnitNames


module FSharpFunctionSandbox =

    module Say =
        let hello name =
            printfn "Hello %s" name

    module WeightConversion =
        //InternationalPound
        let kilogramsPerPound: decimal<kilogram/pound> = 0.45359237m<kilogram/pound> //International pound
        
        //Metric to metric weight
        let gramsPerKilogram : decimal<gram/kilogram> = 1000m<gram/kilogram>
        let milligramsPerGram : decimal<milligram/gram> = 1000m<milligram/gram>
        
        //Imperial weight constants
        let grainsPerPound: decimal<grain/pound> = 7000m<grain/pound>
        let drachmPerPound: decimal<drachm/pound> = 256m<drachm/pound>
        let drachmPerOunce: decimal<drachm/ounce> = 16m<drachm/ounce>
        let ouncesPerPound: decimal<ounce/pound> = 16m<ounce/pound>
        let poundsPerStone: decimal<pound/stone> = 14m<pound/stone>
        let poundsPerQuarterShrt: decimal<pound/quarter_wt_shrt> = 25m<pound/quarter_wt_shrt>
        let stonesPerQuarterLng: decimal<stone/quarter_wt_lng> = 2m<stone/quarter_wt_lng>
        let quartersLngPerHundredweightLng: decimal<quarter_wt_lng/hundredweight_lng> = 4m<quarter_wt_lng/hundredweight_lng>
        let quartersShrtPerHundredweightShrt: decimal<quarter_wt_shrt/hundredweight_shrt> = 4m<quarter_wt_shrt/hundredweight_shrt>
        let poundsPerTonShrt: decimal<pound/ton_shrt> = 2000m<pound/ton_shrt>
        let hundredweightsLngPerTonLng: decimal<hundredweight_lng/ton_lng> = 20m<hundredweight_lng/ton_lng>
        let hundredweightsShrtPerTonShrt: decimal<hundredweight_shrt/ton_shrt> = 20m<hundredweight_shrt/ton_shrt>
        let hundredweightShrtPerhundredweightLng: decimal<hundredweight_shrt/hundredweight_lng> = 1.12m<hundredweight_shrt/hundredweight_lng>


        //Imperial weight convert up
        let grainsToPounds (gr : decimal<grain>) = gr / grainsPerPound
        let drachmToOunce (dr: decimal<drachm>) = dr / drachmPerOunce
        let ouncesToPounds (oz: decimal<ounce>) = oz / ouncesPerPound
        let poundsToStone (lb: decimal<pound>) = lb / poundsPerStone
        let stonesToQuartersLng (st: decimal<stone>) = st / stonesPerQuarterLng
        let poundsToQuartersLng = poundsToStone >> stonesToQuartersLng
        let poundsToQuartersShrt (lb: decimal<pound>) = lb / poundsPerQuarterShrt
        let quartersLngToHundredweightsLng (qtr: decimal<quarter_wt_lng>) = qtr / quartersLngPerHundredweightLng
        let quartersShrtToHundredweightsShrt (qtr: decimal<quarter_wt_shrt>) = qtr / quartersShrtPerHundredweightShrt
        let poundsToHundredweightsLng = poundsToQuartersLng >> quartersLngToHundredweightsLng
        let poundsToHundredweightsShrt = poundsToQuartersShrt >> quartersShrtToHundredweightsShrt
        let hundredweightsLngToTonsLng (cwt: decimal<hundredweight_lng>) = cwt / hundredweightsLngPerTonLng
        let hundredweightsShrtToTonsShrt (cwt: decimal<hundredweight_shrt>) = cwt / hundredweightsShrtPerTonShrt
        let poundsToTonsLng = poundsToHundredweightsLng >> hundredweightsLngToTonsLng
        let poundsToTonsShrt = poundsToHundredweightsShrt >> hundredweightsShrtToTonsShrt
        let hundredweightsShrtToTonShrt (cwt: decimal<hundredweight_shrt>) = cwt / hundredweightsShrtPerTonShrt

        //Imperial weight convert down
        let stoneToPounds (st: decimal<stone>) = st * poundsPerStone
        let quartersShrtToPounds (qtr: decimal<quarter_wt_shrt>) = qtr * poundsPerQuarterShrt
        let quartersLngToStones (qtr: decimal<quarter_wt_lng>) = qtr * stonesPerQuarterLng
        let hundredweightsShrtToQuartersShrt (cwt: decimal<hundredweight_shrt>) = cwt * quartersShrtPerHundredweightShrt
        let hundredweightsLngToQuartersLng (cwt: decimal<hundredweight_lng>) = cwt * quartersLngPerHundredweightLng
        let tonsShrtToHundredweightsShrt (tons: decimal<ton_shrt>) = tons * hundredweightsShrtPerTonShrt
        let tonsLngToHundredweightsLng (t: decimal<ton_lng>) = t * hundredweightsLngPerTonLng
        let tonsLngToQuartersLng = tonsLngToHundredweightsLng >> hundredweightsLngToQuartersLng
        let tonsLngToStones = tonsLngToQuartersLng >> quartersLngToStones

        //Short to Long hundredweight
        let hundredweightsLngToHundredweightsShrt (cwt: decimal<hundredweight_lng>) = cwt * hundredweightShrtPerhundredweightLng
        let hundredweightsShrtToHundredweightsLng (cwt: decimal<hundredweight_shrt>) = cwt / hundredweightShrtPerhundredweightLng
        
        let quartersShrtToHundredweightsLng = quartersShrtToHundredweightsShrt >> hundredweightsShrtToHundredweightsLng
        let quartersShrtToQuartersLng = quartersShrtToHundredweightsLng >> hundredweightsLngToQuartersLng
        let quartersShrtToTonsLng = quartersShrtToHundredweightsLng >> hundredweightsLngToTonsLng
        let quartersShrtToStones = quartersShrtToQuartersLng >> quartersLngToStones

        let quartersLngToHundredweightsShrt = quartersLngToHundredweightsLng >> hundredweightsLngToHundredweightsShrt
        let quartersLngToQuartersShrt = quartersLngToHundredweightsShrt >> hundredweightsShrtToQuartersShrt
        let quartersLngToTonsShrt = quartersLngToHundredweightsShrt >> hundredweightsShrtToTonsShrt

        let hundredweightsShrtToQuartersLng = hundredweightsShrtToHundredweightsLng >> hundredweightsLngToQuartersLng
        let hundredweightsShrtToTonsLng = hundredweightsShrtToHundredweightsLng >> hundredweightsLngToTonsLng
        let hundredweightsShrtToStones = hundredweightsShrtToQuartersLng >> quartersLngToStones

        let hundredweightsLngToQuartersShrt = hundredweightsLngToHundredweightsShrt >> hundredweightsShrtToQuartersShrt
        let hundredweightsLngToTonsShrt = hundredweightsLngToHundredweightsShrt >> hundredweightsShrtToTonsShrt

        let tonsShrtToHundredweightsLng = tonsShrtToHundredweightsShrt >> hundredweightsShrtToHundredweightsLng
        let tonsShrtToQuartersLng = tonsShrtToHundredweightsLng >> hundredweightsLngToQuartersLng
        let tonsShrtToTonsLng = tonsShrtToHundredweightsLng >> hundredweightsLngToTonsLng
        let tonsShrtToStones = tonsShrtToQuartersLng >> quartersLngToStones

        let tonsLngToQuartersShrt = tonsLngToHundredweightsLng >> hundredweightsLngToQuartersShrt
        let tonsLngToHundredweightsShrt = tonsLngToHundredweightsLng >> hundredweightsLngToHundredweightsShrt
        let tonsLngToTonsShrt = tonsLngToHundredweightsShrt >> hundredweightsShrtToTonsShrt

        let stonesToQuartersShrt = stonesToQuartersLng >> quartersLngToQuartersShrt
        let stonesToHundredweightsShrt = stonesToQuartersShrt >> quartersShrtToHundredweightsShrt
        let stonesToTonsShrt = stonesToHundredweightsShrt >> hundredweightsShrtToTonShrt
        

        //Metric Weight convert up
        let mgToG (mg : decimal<milligram>) = mg / milligramsPerGram
        let gToKg (g: decimal<gram>) = g  / gramsPerKilogram
        let mgToKg = mgToG >> gToKg

        //Metric Weight convert down
        let kilogramsToGrams (kg : decimal<kilogram>) = kg * gramsPerKilogram
        let gramsToMilligrams (g : decimal<gram>) = g * milligramsPerGram
        let kilogramsToMilligrams = kilogramsToGrams >> gramsToMilligrams

        //metric to imperial
        let kilogramsToPound (kg: decimal<kilogram>) = kg / kilogramsPerPound
        let poundsToKilogram (lb: decimal<pound>) = lb * kilogramsPerPound

        //imperial to metric
        let grainsToKilogram = grainsToPounds >> poundsToKilogram
        let ouncesToKilogram = ouncesToPounds >> poundsToKilogram
        let drachmToKilogram = drachmToOunce >> ouncesToKilogram
        let stoneToKilogram = stoneToPounds >> poundsToKilogram
        let quartersShrtToKilogram = quartersShrtToPounds >> poundsToKilogram
        let quartersLngToKilogram = quartersLngToStones >> stoneToKilogram
        let hundredweightsShrtToKilogram = hundredweightsShrtToQuartersShrt >> quartersShrtToKilogram
        let hundredweightLngToKilogram = hundredweightsLngToQuartersLng >> quartersLngToKilogram
        let tonsShrtToKilogram = tonsShrtToHundredweightsShrt >> hundredweightsShrtToKilogram
        let tonsLngToKilogram = tonsLngToHundredweightsLng >> hundredweightLngToKilogram


        let grainsToGrams = grainsToKilogram >> kilogramsToGrams
        let ouncesToGrams = ouncesToKilogram >> kilogramsToGrams
        let drachmToGrams = drachmToKilogram >> kilogramsToGrams
        let stoneToGrams = stoneToKilogram >> kilogramsToGrams
        let quartersShrtToGrams = quartersShrtToKilogram >> kilogramsToGrams
        let quartersLngToGrams = quartersLngToKilogram >> kilogramsToGrams
        let hundredweightShrtToGrams = hundredweightsShrtToKilogram >> kilogramsToGrams
        let hundredweightLngToGrams = hundredweightLngToKilogram >> kilogramsToGrams
        let tonsShrtToGrams = tonsShrtToKilogram >> kilogramsToGrams
        let tonsLngToGrams = tonsLngToKilogram >> kilogramsToGrams

        let grainsToMilligrams = grainsToGrams >> gramsToMilligrams
        let ouncesToMilligrams = ouncesToGrams >> gramsToMilligrams
        let drachmToMilligrams = drachmToGrams >> gramsToMilligrams
        let stoneToMilligrams = stoneToGrams >> gramsToMilligrams
        let quartersShrtToMilligrams = quartersShrtToGrams >> gramsToMilligrams
        let quartersLngToMilligrams = quartersLngToGrams >> gramsToMilligrams
        let hundredweightShrtToMilligrams = hundredweightShrtToGrams >> gramsToMilligrams
        let hundredweightLngToMilligrams = hundredweightLngToGrams >> gramsToMilligrams
        let tonsShrtToMilligrams = tonsShrtToGrams >> gramsToMilligrams
        let tonsLngToMilligrams = tonsLngToGrams >> gramsToMilligrams




    module VolumeConversion =
        //based on international yard
        let litersPerUKGallon:decimal<liter/gallon_uk> = 4.5609m<liter/gallon_uk>
        let mlPerFlOzUS:decimal<milliliter/fluidOunce_us> = 29.5735295625m<milliliter/fluidOunce_us>
        let usGallonsPerUkGallon:decimal<gallon_us/gallon_uk> = 1.201m<gallon_us/gallon_uk>
        let cubicInchesPerGallonUs: decimal<cubicInch/gallon_us> = 231m<cubicInch/gallon_us>

        //US Volume
        let minimPerFluidDramUs = 60m<minim_us/fluidDram_us>
        let minimPerTeaspoonUs = 80m<minim_us/teaspoon_us>
        let teaspoonPerTablespoonUs = 3m<teaspoon_us/tablespoon_us>
        let tablespoonPerFluidOunceUs = 2m<tablespoon_us/fluidOunce_us>
        let tablespoonPerShotUs = 3m<tablespoon_us/shot_us>
        let fluidOuncePerGillUs = 4m<fluidOunce_us/gill_us>
        let gillsPerCupUs = 2m<gill_us/cup_us>
        let cupsPerPintUs = 2m<cup_us/pint_us>
        let pintsPerQuartUs = 2m<pint_us/quart_us>
        let quartsPerPottleUs = 2m<quart_us/pottle_us>
        let pottlesPerGallonUs = 2m<pottle_us/gallon_us>
        let gallonsPerBarrelUs = 31.5m<gallon_us/barrel>
        let barrelsPerHogsheadUs = 2m<barrel/hogshead>

        //UK Volume
        let minimPerFluidDrachmUk = 60m<minim_uk/fluidDrachm_uk>
        let fluidDrachmPerFlOzUk = 8m<fluidDrachm_uk/fluidOunce_uk>
        let fluidOuncePerGillUk = 5m<fluidOunce_uk/gill_uk>
        let gillsPerPintUk = 4m<gill_uk/pint_uk>
        let pintsPerQuartUk = 2m<pint_uk/quart_uk>
        let quartsPerGallonUk = 2m<quart_uk/gallon_uk>
        let gallonsPerPeckUk = 2m<gallon_uk/peck_uk>
        let pecksPerBushelUk = 4m<peck_uk/bushel_uk>
        let bushelsPerQuarterUk = 8m<bushel_uk/quarter_vl_uk>

        //Metric
        let millilitersPerLiter: decimal<milliliter/liter> = 1000m<milliliter/liter>

        let literToMilliliter (l: decimal<liter>) = l * millilitersPerLiter
        let milliliterToLiter (ml: decimal<milliliter>) = ml / millilitersPerLiter

        let usGallonToUkGallon (usGallon: decimal<gallon_us>) = usGallon / usGallonsPerUkGallon
        let mlPerUSGallon = mlPerFlOzUS * 128m<fluidOunce_us>

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
        let mmToMeter = mmToCm >> cmToMeter

        //Imperial conversion down
        let yardToFoot (yard: decimal<yard>) = yard * feetPerYard
        let footToInch (foot: decimal<foot>) = foot * inchesPerFoot
        let inchToThou (inch: decimal<inch>) = inch * thousPerInch
        let yardToInch = yardToFoot >> footToInch
        let yardToThou = yardToInch >> inchToThou

        //Metric Length Down
        let meterToCm (meter: decimal<meter>) = meter * cmPerMeter
        let cmToMm (centimeter: decimal<centimeter>) = centimeter * mmPerCm
        let meterToMm = meterToCm >> cmToMm

        //lossy conversions because of irrational division
        //Imperial conversion up
        let thouToInch (thou: decimal<thou>) = thou / thousPerInch
        let inchToFoot (inch: decimal<inch>) = inch / inchesPerFoot
        let footToYard (foot: decimal<foot>) = foot / feetPerYard
        let thouToFoot = thouToInch >> inchToFoot
        let inchToYard = inchToFoot >> footToYard
        let thouToYard = thouToFoot >> footToYard

        let meterToYard (m: decimal<meter>) = m / metersPerYard
        let cmToYard = cmToMeter >> meterToYard
        let cmToFoot = cmToYard >> yardToFoot
        let cmToInch = cmToFoot >> footToInch
        let cmToThou = cmToInch >> inchToThou

        let yardToMeter (yd: decimal<yard>) = yd * metersPerYard

        let thouToMeter = thouToYard >> yardToMeter
        let inchToMeter = inchToYard >> yardToMeter
        let footToMeter = footToYard >> yardToMeter

        





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