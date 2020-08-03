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
        let quartersShrtPerHundredweightsShrt: decimal<quarter_wt_shrt/hundredweight_shrt> = 4m<quarter_wt_shrt/hundredweight_shrt>
        let poundsPerTonShrt: decimal<pound/ton_shrt> = 2000m<pound/ton_shrt>
        let hundredweightsLngPerTonLng: decimal<hundredweight_lng/ton_lng> = 20m<hundredweight_lng/ton_lng>
        let hundredweightsShrtPerTonShrt: decimal<hundredweight_shrt/ton_shrt> = 20m<hundredweight_shrt/ton_shrt>
        let hundredweightsShrtPerhundredweightsLng: decimal<hundredweight_shrt/hundredweight_lng> = 1.12m<hundredweight_shrt/hundredweight_lng>
        let shortTonsPerMetricTons: decimal<ton_shrt/ton_metric> = 0.90718474m<ton_shrt/ton_metric>
        let kilogramsPerMetricTon: decimal<kilogram/ton_metric> = 1000m<kilogram/ton_metric>
        let poundsToOunces (lb: decimal<pound>) = lb * ouncesPerPound 

        //Imperial weight convert up
        let grainsToPounds (gr : decimal<grain>) = gr / grainsPerPound
        let drachmsToOunces (dr: decimal<drachm>) = dr / drachmPerOunce
        let ouncesToPounds (oz: decimal<ounce>) = oz / ouncesPerPound
        let poundsToStones (lb: decimal<pound>) = lb / poundsPerStone
        let stonesToQuartersLng (st: decimal<stone>) = st / stonesPerQuarterLng
        let poundsToQuartersLng = poundsToStones >> stonesToQuartersLng
        let poundsToQuartersShrt (lb: decimal<pound>) = lb / poundsPerQuarterShrt
        let quartersLngToHundredweightsLng (qtr: decimal<quarter_wt_lng>) = qtr / quartersLngPerHundredweightLng
        let quartersShrtToHundredweightsShrt (qtr: decimal<quarter_wt_shrt>) = qtr / quartersShrtPerHundredweightsShrt
        let poundsToHundredweightsLng = poundsToQuartersLng >> quartersLngToHundredweightsLng
        let poundsToHundredweightsShrt = poundsToQuartersShrt >> quartersShrtToHundredweightsShrt
        let hundredweightsLngToTonsLng (cwt: decimal<hundredweight_lng>) = cwt / hundredweightsLngPerTonLng
        let hundredweightsShrtToTonsShrt (cwt: decimal<hundredweight_shrt>) = cwt / hundredweightsShrtPerTonShrt
        let poundsToTonsLng = poundsToHundredweightsLng >> hundredweightsLngToTonsLng
        let poundsToTonsShrt = poundsToHundredweightsShrt >> hundredweightsShrtToTonsShrt
        let ouncesToDrachms (oz: decimal<ounce>) = oz * drachmPerOunce

        //Imperial weight convert down
        let poundsToGrains (lb: decimal<pound>) = lb * grainsPerPound
        let stonesToPounds (st: decimal<stone>) = st * poundsPerStone
        let quartersShrtToPounds (qtr: decimal<quarter_wt_shrt>) = qtr * poundsPerQuarterShrt
        let quartersLngToStones (qtr: decimal<quarter_wt_lng>) = qtr * stonesPerQuarterLng
        let hundredweightsShrtToQuartersShrt (cwt: decimal<hundredweight_shrt>) = cwt * quartersShrtPerHundredweightsShrt
        let hundredweightsLngToQuartersLng (cwt: decimal<hundredweight_lng>) = cwt * quartersLngPerHundredweightLng
        let tonsShrtToHundredweightsShrt (tons: decimal<ton_shrt>) = tons * hundredweightsShrtPerTonShrt
        let tonsLngToHundredweightsLng (t: decimal<ton_lng>) = t * hundredweightsLngPerTonLng
        let tonsLngToQuartersLng = tonsLngToHundredweightsLng >> hundredweightsLngToQuartersLng
        let tonsLngToStones = tonsLngToQuartersLng >> quartersLngToStones

        //Short to Long hundredweight
        let hundredweightsLngToHundredweightsShrt (cwt: decimal<hundredweight_lng>) = cwt * hundredweightsShrtPerhundredweightsLng
        let hundredweightsShrtToHundredweightsLng (cwt: decimal<hundredweight_shrt>) = cwt / hundredweightsShrtPerhundredweightsLng
        //let shortTonsToMetricTons (t: decimal<ton_shrt>) = t / shortTonsPerMetricTons

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
        let stonesToTonsShrt = stonesToHundredweightsShrt >> hundredweightsShrtToTonsShrt

        //metric to imperial
        let kilogramsToPounds (kg: decimal<kilogram>) = kg / kilogramsPerPound
        let poundsToKilograms (lb: decimal<pound>) = lb * kilogramsPerPound
 
        let kilogramsToOunces = kilogramsToPounds >> poundsToOunces

        //Metric Weight convert up
        let milligramsToGrams (mg : decimal<milligram>) = mg / milligramsPerGram
        let gramsToKilograms (g: decimal<gram>) = g  / gramsPerKilogram
        let kilogramsToTonsMetric (kg: decimal<kilogram>) = kg / kilogramsPerMetricTon
        let metricTonsToKilograms (t: decimal<ton_metric>) = t * kilogramsPerMetricTon
        let milligramsToKilograms = milligramsToGrams >> gramsToKilograms

        //let milligramsToDrachms = milligramsToOunces >> ouncesToDrachms

        //Metric Weight convert down
        let kilogramsToGrams (kg : decimal<kilogram>) = kg * gramsPerKilogram
        let gramsToMilligrams (g : decimal<gram>) = g * milligramsPerGram
        let kilogramsToMilligrams = kilogramsToGrams >> gramsToMilligrams


        //imperial to metric
        let grainsToKilograms = grainsToPounds >> poundsToKilograms
        let ouncesToKilograms = ouncesToPounds >> poundsToKilograms
        let drachmsToKilograms = drachmsToOunces >> ouncesToKilograms
        let stonesToKilograms = stonesToPounds >> poundsToKilograms
        let quartersShrtToKilograms = quartersShrtToPounds >> poundsToKilograms
        let quartersLngToKilograms = quartersLngToStones >> stonesToKilograms
        let hundredweightsShrtToKilograms = hundredweightsShrtToQuartersShrt >> quartersShrtToKilograms
        let hundredweightsLngToKilograms = hundredweightsLngToQuartersLng >> quartersLngToKilograms
        let tonsShrtToKilograms = tonsShrtToHundredweightsShrt >> hundredweightsShrtToKilograms
        let tonsLngToKilograms = tonsLngToHundredweightsLng >> hundredweightsLngToKilograms
        let tonsShrtToTonsMetric = tonsShrtToKilograms >> kilogramsToTonsMetric
        let tonsLongToTonsMetric = tonsLngToKilograms >> kilogramsToTonsMetric

        let grainsToGrams = grainsToKilograms >> kilogramsToGrams
        let ouncesToGrams = ouncesToKilograms >> kilogramsToGrams
        let drachmsToGrams = drachmsToKilograms >> kilogramsToGrams
        let stoneToGrams = stonesToKilograms >> kilogramsToGrams
        let quartersShrtToGrams = quartersShrtToKilograms >> kilogramsToGrams
        let quartersLngToGrams = quartersLngToKilograms >> kilogramsToGrams
        let hundredweightsShrtToGrams = hundredweightsShrtToKilograms >> kilogramsToGrams
        let hundredweightsLngToGrams = hundredweightsLngToKilograms >> kilogramsToGrams
        let tonsShrtToGrams = tonsShrtToKilograms >> kilogramsToGrams
        let tonsLngToGrams = tonsLngToKilograms >> kilogramsToGrams

        let grainsToMilligrams = grainsToGrams >> gramsToMilligrams
        let ouncesToMilligrams = ouncesToGrams >> gramsToMilligrams
        let drachmToMilligrams = drachmsToGrams >> gramsToMilligrams
        let stoneToMilligrams = stoneToGrams >> gramsToMilligrams
        let quartersShrtToMilligrams = quartersShrtToGrams >> gramsToMilligrams
        let quartersLngToMilligrams = quartersLngToGrams >> gramsToMilligrams
        let hundredweightsShrtToMilligrams = hundredweightsShrtToGrams >> gramsToMilligrams
        let hundredweightsLngToMilligrams = hundredweightsLngToGrams >> gramsToMilligrams
        let tonsShrtToMilligrams = tonsShrtToGrams >> gramsToMilligrams
        let tonsLngToMilligrams = tonsLngToGrams >> gramsToMilligrams
        let poundsToTonsMetric = poundsToKilograms >> kilogramsToTonsMetric

        let milligramsToPounds = milligramsToKilograms >> kilogramsToPounds
        let milligramsToGrains = milligramsToPounds >> poundsToGrains
        let milligramsToStone = milligramsToPounds >> poundsToStones
        let milligramsToOunce = milligramsToPounds>> poundsToOunces
        let milligramsToDrachms = milligramsToPounds >> poundsToOunces >> ouncesToDrachms
        let milligramsToStones = milligramsToPounds >> poundsToStones
        let milligramsToQuartersLng = milligramsToPounds >> poundsToQuartersLng
        let milligramsToQuartersShrt = milligramsToPounds >> poundsToQuartersShrt
        let milligramsToHundredweightsLng = milligramsToQuartersLng >> quartersLngToHundredweightsLng
        let milligramsToHundredweightsShrt = milligramsToQuartersShrt >> quartersShrtToHundredweightsShrt
        let milligramsToTonsLng = milligramsToHundredweightsLng >> hundredweightsLngToTonsLng
        let milligramsToTonsShrt = milligramsToHundredweightsShrt >> hundredweightsShrtToTonsShrt
        let milligramsToTonsMetric = milligramsToKilograms >> kilogramsToTonsMetric

        let gramsToPounds = gramsToKilograms >> kilogramsToPounds
        let gramsToGrains = gramsToPounds >> poundsToGrains
        let gramsToOunces = gramsToPounds >> poundsToOunces
        let gramsToDrachms = gramsToOunces >> ouncesToDrachms
        let gramsToStones = gramsToPounds >> poundsToStones
        let gramsToQuartersLng = gramsToPounds >> poundsToQuartersLng
        let gramsToQuartersShrt = gramsToPounds >> poundsToQuartersShrt
        let gramsToHundredweightsLng = gramsToPounds >> poundsToHundredweightsLng
        let gramsToHundredweightsShrt = gramsToPounds >> poundsToHundredweightsShrt
        let gramsToTonsLng = gramsToPounds >> poundsToTonsLng
        let gramsToTonsShrt = gramsToPounds >> poundsToTonsShrt
        let gramsToTonsMetric = gramsToKilograms >> kilogramsToTonsMetric

        let kilogramsToGrains = kilogramsToGrams >> gramsToGrains
        let kilogramsToDrachms = kilogramsToGrams >> gramsToDrachms
        let kilogramsToStones = kilogramsToGrams >> gramsToStones
        let kilogramsToQuartersLng = kilogramsToGrams >> gramsToQuartersLng
        let kilogramsToQuartersShrt = kilogramsToGrams >> gramsToQuartersShrt
        let kilogramsToHundredweightsLng = kilogramsToGrams >> gramsToHundredweightsLng
        let kilogramsToHundredweightsShrt = kilogramsToGrams >> gramsToHundredweightsShrt
        let kilogramsToTonsLng = kilogramsToGrams >> gramsToTonsLng
        let kilogramsToTonsShrt = kilogramsToGrams >> gramsToTonsShrt

        let metricTonsToGrams = metricTonsToKilograms >> kilogramsToGrams
        let metricTonsToMilligrams = metricTonsToGrams >> gramsToMilligrams
        let metricTonsToGrains = metricTonsToKilograms >> kilogramsToGrains
        let metricTonsToDrachms = metricTonsToKilograms >> kilogramsToDrachms
        let metricTonsToOunces = metricTonsToKilograms >> kilogramsToOunces
        let metricTonsToPounds = metricTonsToKilograms >> kilogramsToPounds
        let metricTonsToStones = metricTonsToKilograms >> kilogramsToStones
        let metricTonsToQuartersShrt = metricTonsToKilograms >> kilogramsToQuartersShrt
        let metricTonsToQuartersLng = metricTonsToKilograms >> kilogramsToQuartersLng
        let metricTonsToHundredweightLng = metricTonsToKilograms >> kilogramsToHundredweightsLng
        let metricTonsToHundredweightShrt = metricTonsToKilograms >> kilogramsToHundredweightsShrt
        let metricTonsToTonLng = metricTonsToKilograms >> kilogramsToTonsLng
        let metricTonsToTonShrt = metricTonsToKilograms >> kilogramsToTonsShrt

        
        let grainsToMetricTons = grainsToKilograms >> kilogramsToTonsMetric
        let grainsToOunces = grainsToPounds >> poundsToOunces
        let grainsToDrachms = grainsToOunces >> ouncesToDrachms
        let grainsToStones = grainsToPounds >> poundsToStones
        let grainsToQuartersLng = grainsToStones >> stonesToQuartersLng
        let grainsToQuartersShrt = grainsToStones >> stonesToQuartersShrt
        let grainsToHundredweightsShrt = grainsToQuartersShrt >> quartersShrtToHundredweightsShrt
        let grainsToHundredweightsLng = grainsToQuartersLng >> quartersLngToHundredweightsLng
        let grainsToTonsShrt = grainsToHundredweightsShrt >> hundredweightsShrtToTonsShrt
        let grainsToTonsLng = grainsToHundredweightsLng >> hundredweightsLngToTonsLng
        let grainsToTonsMetric = grainsToKilograms >> kilogramsToTonsMetric

        let drachmsToMetricTons = drachmsToKilograms >> kilogramsToTonsMetric
        let drachmsToPounds =  drachmsToOunces >> ouncesToPounds
        let drachmsToGrains = drachmsToPounds >> poundsToGrains
        let drachmsToStones = drachmsToPounds >> poundsToStones
        let drachmsToQuartersShrt = drachmsToPounds >> poundsToQuartersShrt
        let drachmsToQuartersLng = drachmsToStones >> stonesToQuartersLng
        let drachmsToHundredweightsShrt = drachmsToQuartersShrt >> quartersShrtToHundredweightsShrt
        let drachmsToHundredweightsLng = drachmsToQuartersLng >> quartersLngToHundredweightsLng
        let drachmsToTonsShrt = drachmsToHundredweightsShrt >> hundredweightsShrtToTonsShrt
        let drachmsToTonsLng = drachmsToHundredweightsLng >> hundredweightsLngToTonsLng
        let drachmsToTonsMetric = drachmsToKilograms >> kilogramsToTonsMetric

        let ouncesToGrains = ouncesToPounds >> poundsToGrains
        let ouncesToStones = ouncesToPounds >> poundsToStones
        let ouncesToQuartersShrt = ouncesToPounds >> poundsToQuartersShrt
        let ouncesToQuartersLng = ouncesToStones >> stonesToQuartersLng
        let ouncesToHundredweightsShrt = ouncesToQuartersShrt >> quartersShrtToHundredweightsShrt
        let ouncesToHundredweightsLng = ouncesToQuartersLng >> quartersLngToHundredweightsLng
        let ouncesToTonsShrt = ouncesToHundredweightsShrt >> hundredweightsShrtToTonsShrt
        let ouncesToTonsLng = ouncesToHundredweightsLng >> hundredweightsLngToTonsLng
        let ouncesToTonsMetric = ouncesToKilograms >> kilogramsToTonsMetric

        let poundsToMilligrams = poundsToKilograms >> kilogramsToMilligrams
        let poundsToGrams = poundsToKilograms >> kilogramsToGrams
        let poundsToDrachms = poundsToGrains >> grainsToDrachms

        let stonesToMilligrams = stonesToKilograms >> kilogramsToMilligrams
        let stonesToGrams = stonesToKilograms >> kilogramsToGrams
        let stonesToTonsMetric = stonesToKilograms >> kilogramsToTonsMetric
        let stonesToGrains = stonesToPounds >> poundsToGrains
        let stonesToDrachms = stonesToPounds >> poundsToDrachms
        let stonesToOunces = stonesToPounds >> poundsToOunces
        let stonesToHundredweightsLng = stonesToQuartersLng >> quartersLngToHundredweightsLng
        let stonesToTonsLng = stonesToHundredweightsLng >> hundredweightsLngToTonsLng

        let quartersShrtToTonsMetric = quartersShrtToKilograms >> kilogramsToTonsMetric
        let quartersShrtToGrains = quartersShrtToPounds >> poundsToGrains
        let quartersShrtToDrachms = quartersShrtToPounds >> poundsToDrachms
        let quartersShrtToOunces = quartersShrtToPounds >> poundsToOunces
        let quartersShrtToTonsShrt = quartersShrtToHundredweightsShrt >> hundredweightsShrtToTonsShrt

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