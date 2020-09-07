namespace FSharpFunctionSandbox

open Units.Imperial.UnitNames
open Units.SI.UnitNames


module FSharpFunctionSandbox =

    module Dilution =
        let diluteGetTotalSG startingSg endingSg startingVolume endingVolume = ((startingVolume * startingSg) + (endingVolume * endingSg))/startingVolume+endingVolume
        let diluteFindBegVolume endingVolume endingSg startingSg = (endingVolume * endingSg) / startingSg
        let diluteFindBegSg endingVolume endingSg startingVolume = (endingVolume * endingSg) / startingVolume
        let diluteFindEndVolume startingVolume startingSg endingSg = (startingVolume * startingSg) /endingSg
        let diluteFindEndSg startingVolume beginningSg endingVolume = (startingVolume * beginningSg) / endingVolume

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
        let poundsToQuartersLng x = poundsToStones x |> stonesToQuartersLng
        let poundsToQuartersShrt (lb: decimal<pound>) = lb / poundsPerQuarterShrt
        let quartersLngToHundredweightsLng (qtr: decimal<quarter_wt_lng>) = qtr / quartersLngPerHundredweightLng
        let quartersShrtToHundredweightsShrt (qtr: decimal<quarter_wt_shrt>) = qtr / quartersShrtPerHundredweightsShrt
        let poundsToHundredweightsLng x = poundsToQuartersLng x |> quartersLngToHundredweightsLng
        let poundsToHundredweightsShrt x = poundsToQuartersShrt x |> quartersShrtToHundredweightsShrt
        let hundredweightsLngToTonsLng (cwt: decimal<hundredweight_lng>) = cwt / hundredweightsLngPerTonLng
        let hundredweightsShrtToTonsShrt (cwt: decimal<hundredweight_shrt>) = cwt / hundredweightsShrtPerTonShrt
        let poundsToTonsLng x = poundsToHundredweightsLng x |> hundredweightsLngToTonsLng
        let poundsToTonsShrt x = poundsToHundredweightsShrt x |> hundredweightsShrtToTonsShrt
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
        let tonsLngToQuartersLng x = tonsLngToHundredweightsLng x |> hundredweightsLngToQuartersLng
        let tonsLngToStones x = tonsLngToQuartersLng x |> quartersLngToStones

        //Short to Long hundredweight
        let hundredweightsLngToHundredweightsShrt (cwt: decimal<hundredweight_lng>) = cwt * hundredweightsShrtPerhundredweightsLng
        let hundredweightsShrtToHundredweightsLng (cwt: decimal<hundredweight_shrt>) = cwt / hundredweightsShrtPerhundredweightsLng
        //let shortTonsToMetricTons (t: decimal<ton_shrt>) = t / shortTonsPerMetricTons

        let quartersShrtToHundredweightsLng x = quartersShrtToHundredweightsShrt x |> hundredweightsShrtToHundredweightsLng
        let quartersShrtToQuartersLng x = quartersShrtToHundredweightsLng x |> hundredweightsLngToQuartersLng
        let quartersShrtToTonsLng x = quartersShrtToHundredweightsLng x |> hundredweightsLngToTonsLng
        let quartersShrtToStones x = quartersShrtToQuartersLng x |> quartersLngToStones

        let quartersLngToHundredweightsShrt x = quartersLngToHundredweightsLng x |> hundredweightsLngToHundredweightsShrt
        let quartersLngToQuartersShrt x= quartersLngToHundredweightsShrt x |> hundredweightsShrtToQuartersShrt
        let quartersLngToTonsShrt x = quartersLngToHundredweightsShrt x |> hundredweightsShrtToTonsShrt

        let hundredweightsShrtToQuartersLng x = hundredweightsShrtToHundredweightsLng x |> hundredweightsLngToQuartersLng
        let hundredweightsShrtToTonsLng x = hundredweightsShrtToHundredweightsLng x |> hundredweightsLngToTonsLng
        let hundredweightsShrtToStones x = hundredweightsShrtToQuartersLng x |> quartersLngToStones

        let hundredweightsLngToQuartersShrt x = hundredweightsLngToHundredweightsShrt x |> hundredweightsShrtToQuartersShrt
        let hundredweightsLngToTonsShrt x = hundredweightsLngToHundredweightsShrt x |> hundredweightsShrtToTonsShrt

        let tonsShrtToHundredweightsLng x = tonsShrtToHundredweightsShrt x |> hundredweightsShrtToHundredweightsLng
        let tonsShrtToQuartersLng x = tonsShrtToHundredweightsLng x |> hundredweightsLngToQuartersLng
        let tonsShrtToTonsLng x = tonsShrtToHundredweightsLng x |> hundredweightsLngToTonsLng
        let tonsShrtToStones x = tonsShrtToQuartersLng x |> quartersLngToStones

        let tonsLngToQuartersShrt x = tonsLngToHundredweightsLng x |> hundredweightsLngToQuartersShrt
        let tonsLngToHundredweightsShrt x = tonsLngToHundredweightsLng x |> hundredweightsLngToHundredweightsShrt
        let tonsLngToTonsShrt x = tonsLngToHundredweightsShrt x |> hundredweightsShrtToTonsShrt

        let stonesToQuartersShrt x = stonesToQuartersLng x |> quartersLngToQuartersShrt
        let stonesToHundredweightsShrt x = stonesToQuartersShrt x |> quartersShrtToHundredweightsShrt
        let stonesToTonsShrt x = stonesToHundredweightsShrt x |> hundredweightsShrtToTonsShrt

        //metric to imperial
        let kilogramsToPounds (kg: decimal<kilogram>) = kg / kilogramsPerPound
        let poundsToKilograms (lb: decimal<pound>) = lb * kilogramsPerPound
 
        let kilogramsToOunces x = kilogramsToPounds x |> poundsToOunces

        //Metric Weight convert up
        let milligramsToGrams (mg : decimal<milligram>) = mg / milligramsPerGram
        let gramsToKilograms (g: decimal<gram>) = g  / gramsPerKilogram
        let kilogramsToTonsMetric (kg: decimal<kilogram>) = kg / kilogramsPerMetricTon
        let metricTonsToKilograms (t: decimal<ton_metric>) = t * kilogramsPerMetricTon
        let milligramsToKilograms x = milligramsToGrams x |> gramsToKilograms

        //let milligramsToDrachms = milligramsToOunces |> ouncesToDrachms

        //Metric Weight convert down
        let kilogramsToGrams (kg : decimal<kilogram>) = kg * gramsPerKilogram
        let gramsToMilligrams (g : decimal<gram>) = g * milligramsPerGram
        let kilogramsToMilligrams x = kilogramsToGrams x |> gramsToMilligrams


        //imperial to metric
        let grainsToKilograms x = grainsToPounds x |> poundsToKilograms
        let ouncesToKilograms x = ouncesToPounds x |> poundsToKilograms
        let drachmsToKilograms x = drachmsToOunces x |> ouncesToKilograms
        let stonesToKilograms x = stonesToPounds x |> poundsToKilograms
        let quartersShrtToKilograms x = quartersShrtToPounds x |> poundsToKilograms
        let quartersLngToKilograms x = quartersLngToStones x |> stonesToKilograms
        let hundredweightsShrtToKilograms x = hundredweightsShrtToQuartersShrt x |> quartersShrtToKilograms
        let hundredweightsLngToKilograms x = hundredweightsLngToQuartersLng x |> quartersLngToKilograms
        let tonsShrtToKilograms x = tonsShrtToHundredweightsShrt x |> hundredweightsShrtToKilograms
        let tonsLngToKilograms x = tonsLngToHundredweightsLng x |> hundredweightsLngToKilograms
        let tonsShrtToTonsMetric x = tonsShrtToKilograms x |> kilogramsToTonsMetric

        let grainsToGrams x = grainsToKilograms x |> kilogramsToGrams
        let ouncesToGrams x = ouncesToKilograms x |> kilogramsToGrams
        let drachmsToGrams x = drachmsToKilograms x |> kilogramsToGrams
        let quartersShrtToGrams x = quartersShrtToKilograms x |> kilogramsToGrams
        let quartersLngToGrams x = quartersLngToKilograms x |> kilogramsToGrams
        let hundredweightsShrtToGrams x = hundredweightsShrtToKilograms x |> kilogramsToGrams
        let hundredweightsLngToGrams x = hundredweightsLngToKilograms x |> kilogramsToGrams
        let tonsShrtToGrams x = tonsShrtToKilograms x |> kilogramsToGrams
        let tonsLngToGrams x = tonsLngToKilograms x |> kilogramsToGrams

        let grainsToMilligrams x = grainsToGrams x |> gramsToMilligrams
        let ouncesToMilligrams x = ouncesToGrams x |> gramsToMilligrams
        let drachmToMilligrams x = drachmsToGrams x |> gramsToMilligrams
        let quartersShrtToMilligrams x = quartersShrtToGrams x |> gramsToMilligrams
        let quartersLngToMilligrams x = quartersLngToGrams x |> gramsToMilligrams
        let hundredweightsShrtToMilligrams x = hundredweightsShrtToGrams x |> gramsToMilligrams
        let hundredweightsLngToMilligrams x = hundredweightsLngToGrams x |> gramsToMilligrams
        let tonsShrtToMilligrams x = tonsShrtToGrams x |> gramsToMilligrams
        let tonsLngToMilligrams x = tonsLngToGrams x |> gramsToMilligrams
        let poundsToTonsMetric x = poundsToKilograms x |> kilogramsToTonsMetric

        let milligramsToPounds x = milligramsToKilograms x |> kilogramsToPounds
        let milligramsToGrains x = milligramsToPounds x |> poundsToGrains
        let milligramsToStone x = milligramsToPounds x |> poundsToStones
        let milligramsToOunce x = milligramsToPounds x |> poundsToOunces
        let milligramsToDrachms x = milligramsToPounds x |> poundsToOunces |> ouncesToDrachms
        let milligramsToStones x = milligramsToPounds x |> poundsToStones
        let milligramsToQuartersLng x = milligramsToPounds x |> poundsToQuartersLng
        let milligramsToQuartersShrt x = milligramsToPounds x |> poundsToQuartersShrt
        let milligramsToHundredweightsLng x = milligramsToQuartersLng x |> quartersLngToHundredweightsLng
        let milligramsToHundredweightsShrt x = milligramsToQuartersShrt x |> quartersShrtToHundredweightsShrt
        let milligramsToTonsLng x = milligramsToHundredweightsLng x |> hundredweightsLngToTonsLng
        let milligramsToTonsShrt x = milligramsToHundredweightsShrt x |> hundredweightsShrtToTonsShrt
        let milligramsToTonsMetric x = milligramsToKilograms x |> kilogramsToTonsMetric

        let gramsToPounds x = gramsToKilograms x |> kilogramsToPounds
        let gramsToGrains x = gramsToPounds x |> poundsToGrains
        let gramsToOunces x = gramsToPounds x |> poundsToOunces
        let gramsToDrachms x = gramsToOunces x |> ouncesToDrachms
        let gramsToStones x = gramsToPounds x |> poundsToStones
        let gramsToQuartersLng x = gramsToPounds x |> poundsToQuartersLng
        let gramsToQuartersShrt x = gramsToPounds x |> poundsToQuartersShrt
        let gramsToHundredweightsLng x = gramsToPounds x |> poundsToHundredweightsLng
        let gramsToHundredweightsShrt x = gramsToPounds x |> poundsToHundredweightsShrt
        let gramsToTonsLng x = gramsToPounds x |> poundsToTonsLng
        let gramsToTonsShrt x = gramsToPounds x |> poundsToTonsShrt
        let gramsToTonsMetric x = gramsToKilograms x |> kilogramsToTonsMetric

        let kilogramsToGrains x = kilogramsToGrams x |> gramsToGrains
        let kilogramsToDrachms x = kilogramsToGrams x |> gramsToDrachms
        let kilogramsToStones x = kilogramsToGrams x |> gramsToStones
        let kilogramsToQuartersLng x = kilogramsToGrams x |> gramsToQuartersLng
        let kilogramsToQuartersShrt x = kilogramsToGrams x |> gramsToQuartersShrt
        let kilogramsToHundredweightsLng x = kilogramsToGrams x |> gramsToHundredweightsLng
        let kilogramsToHundredweightsShrt x = kilogramsToGrams x |> gramsToHundredweightsShrt
        let kilogramsToTonsLng x = kilogramsToGrams x |> gramsToTonsLng
        let kilogramsToTonsShrt x = kilogramsToGrams x |> gramsToTonsShrt

        let metricTonsToGrams x = metricTonsToKilograms x |> kilogramsToGrams
        let metricTonsToMilligrams x = metricTonsToGrams x |> gramsToMilligrams
        let metricTonsToGrains x = metricTonsToKilograms x |> kilogramsToGrains
        let metricTonsToDrachms x = metricTonsToKilograms x |> kilogramsToDrachms
        let metricTonsToOunces x = metricTonsToKilograms x |> kilogramsToOunces
        let metricTonsToPounds x = metricTonsToKilograms x |> kilogramsToPounds
        let metricTonsToStones x = metricTonsToKilograms x |> kilogramsToStones
        let metricTonsToQuartersShrt x = metricTonsToKilograms x |> kilogramsToQuartersShrt
        let metricTonsToQuartersLng x = metricTonsToKilograms x |> kilogramsToQuartersLng
        let metricTonsToHundredweightLng x = metricTonsToKilograms x |> kilogramsToHundredweightsLng
        let metricTonsToHundredweightShrt x = metricTonsToKilograms x |> kilogramsToHundredweightsShrt
        let metricTonsToTonLng x = metricTonsToKilograms x |> kilogramsToTonsLng
        let metricTonsToTonShrt x = metricTonsToKilograms x |> kilogramsToTonsShrt

        
        let grainsToMetricTons x = grainsToKilograms x |> kilogramsToTonsMetric
        let grainsToOunces x = grainsToPounds x |> poundsToOunces
        let grainsToDrachms x = grainsToOunces x |> ouncesToDrachms
        let grainsToStones x = grainsToPounds x |> poundsToStones
        let grainsToQuartersLng x = grainsToStones x |> stonesToQuartersLng
        let grainsToQuartersShrt x = grainsToStones x |> stonesToQuartersShrt
        let grainsToHundredweightsShrt x = grainsToQuartersShrt x |> quartersShrtToHundredweightsShrt
        let grainsToHundredweightsLng x = grainsToQuartersLng x |> quartersLngToHundredweightsLng
        let grainsToTonsShrt x = grainsToHundredweightsShrt x |> hundredweightsShrtToTonsShrt
        let grainsToTonsLng x = grainsToHundredweightsLng x |> hundredweightsLngToTonsLng
        let grainsToTonsMetric x = grainsToKilograms x |> kilogramsToTonsMetric

        let drachmsToMetricTons x = drachmsToKilograms x |> kilogramsToTonsMetric
        let drachmsToPounds x =  drachmsToOunces x |> ouncesToPounds
        let drachmsToGrains x = drachmsToPounds x |> poundsToGrains
        let drachmsToStones x = drachmsToPounds x |> poundsToStones
        let drachmsToQuartersShrt x = drachmsToPounds x |> poundsToQuartersShrt
        let drachmsToQuartersLng x = drachmsToStones x |> stonesToQuartersLng
        let drachmsToHundredweightsShrt x = drachmsToQuartersShrt x |> quartersShrtToHundredweightsShrt
        let drachmsToHundredweightsLng x = drachmsToQuartersLng x |> quartersLngToHundredweightsLng
        let drachmsToTonsShrt x = drachmsToHundredweightsShrt x |> hundredweightsShrtToTonsShrt
        let drachmsToTonsLng x = drachmsToHundredweightsLng x |> hundredweightsLngToTonsLng
        let drachmsToTonsMetric x = drachmsToKilograms x |> kilogramsToTonsMetric

        let ouncesToGrains x = ouncesToPounds x |> poundsToGrains
        let ouncesToStones x = ouncesToPounds x |> poundsToStones
        let ouncesToQuartersShrt x = ouncesToPounds x |> poundsToQuartersShrt
        let ouncesToQuartersLng x = ouncesToStones x |> stonesToQuartersLng
        let ouncesToHundredweightsShrt x = ouncesToQuartersShrt x |> quartersShrtToHundredweightsShrt
        let ouncesToHundredweightsLng x = ouncesToQuartersLng x |> quartersLngToHundredweightsLng
        let ouncesToTonsShrt x = ouncesToHundredweightsShrt x |> hundredweightsShrtToTonsShrt
        let ouncesToTonsLng x = ouncesToHundredweightsLng x |> hundredweightsLngToTonsLng
        let ouncesToTonsMetric x = ouncesToKilograms x |> kilogramsToTonsMetric

        let poundsToMilligrams x = poundsToKilograms x |> kilogramsToMilligrams
        let poundsToGrams x = poundsToKilograms x |> kilogramsToGrams
        let poundsToDrachms x = poundsToGrains x |> grainsToDrachms

        let stonesToMilligrams x = stonesToKilograms x |> kilogramsToMilligrams
        let stonesToGrams x = stonesToKilograms x |> kilogramsToGrams
        let stonesToTonsMetric x = stonesToKilograms x |> kilogramsToTonsMetric
        let stonesToGrains x = stonesToPounds x |> poundsToGrains
        let stonesToDrachms x = stonesToPounds x |> poundsToDrachms
        let stonesToOunces x = stonesToPounds x |> poundsToOunces
        let stonesToHundredweightsLng x = stonesToQuartersLng x |> quartersLngToHundredweightsLng
        let stonesToTonsLng x = stonesToHundredweightsLng x |> hundredweightsLngToTonsLng

        let quartersShrtToTonsMetric x = quartersShrtToKilograms x |> kilogramsToTonsMetric
        let quartersShrtToGrains x = quartersShrtToPounds x |> poundsToGrains
        let quartersShrtToDrachms x = quartersShrtToPounds x |> poundsToDrachms
        let quartersShrtToOunces x = quartersShrtToPounds x |> poundsToOunces
        let quartersShrtToTonsShrt x = quartersShrtToHundredweightsShrt x |> hundredweightsShrtToTonsShrt

        let quartersLngToTonsMetric x = quartersLngToKilograms x |> kilogramsToTonsMetric
        let quartersLngToPounds x = quartersLngToStones x |> stonesToPounds
        let quartersLngToGrains x = quartersLngToPounds x |> poundsToGrains
        let quartersLngToDrachms x = quartersLngToPounds x |> poundsToDrachms
        let quartersLngToOunces x = quartersLngToPounds x |> poundsToOunces
        let quartersLngToTonsLng x = quartersLngToHundredweightsLng x |> hundredweightsLngToTonsLng

        let hundredweightsShrtToTonsMetric x = hundredweightsShrtToKilograms x |> kilogramsToTonsMetric
        let hundredweightsShrtToPounds x = hundredweightsShrtToQuartersShrt x |> quartersShrtToPounds
        let hundredweightsShrtToGrains x = hundredweightsShrtToPounds x |> poundsToGrains
        let hundredweightsShrtToDrachms x = hundredweightsShrtToPounds x |> poundsToDrachms
        let hundredweightsShrtToOunces x = hundredweightsShrtToPounds x |> poundsToOunces

        let hundredweightsLngToTonsMetric x = hundredweightsLngToKilograms x |> kilogramsToTonsMetric
        let hundredweightsLngToPounds x = hundredweightsLngToQuartersLng x |> quartersLngToPounds
        let hundredweightsLngToGrains x = hundredweightsLngToPounds x |> poundsToGrains
        let hundredweightsLngToDrachms x = hundredweightsLngToPounds x |> poundsToDrachms
        let hundredweightsLngToOunces x = hundredweightsLngToPounds x |> poundsToOunces
        let hundredweightsLngToStones x = hundredweightsLngToPounds x |> poundsToStones

        let tonsShrtToPounds x = tonsShrtToStones x |> stonesToPounds
        let tonsShrtToGrains x = tonsShrtToPounds x |> poundsToGrains
        let tonsShrtToDrachms x = tonsShrtToPounds x |> poundsToDrachms
        let tonsShrtToOunces x = tonsShrtToPounds x |> poundsToOunces
        let tonsShrtToQuartersShrt x = tonsShrtToHundredweightsShrt x |> hundredweightsShrtToQuartersShrt

        let tonsLngToTonsMetric x = tonsLngToKilograms x |> kilogramsToTonsMetric
        let tonsLngToPounds x = tonsLngToStones x |> stonesToPounds
        let tonsLngToGrains x = tonsLngToPounds x |> poundsToGrains
        let tonsLngToDrachms x = tonsLngToPounds x |> poundsToDrachms
        let tonsLngToOunces x = tonsLngToPounds x |> poundsToOunces


    module VolumeConversion =
        //based on international yard
        let litersPerUKGallon:decimal<liter/gallon_uk> = 4.54609m<liter/gallon_uk>
        let litersPerUSGallon:decimal<liter/gallon_us> = 3.785411784m<liter/gallon_us>
        let usGallonsPerUkGallon:decimal<gallon_us/gallon_uk> = 1.201m<gallon_us/gallon_uk>
        let cubicInchesPerGallonUs: decimal<cubicInch/gallon_us> = 231m<cubicInch/gallon_us>

        //US Volume
        let minimPerFluidDramUs = 60m<minim_us/fluidDram_us>
        let minimPerTeaspoon = 80m<minim_us/teaspoon>
        let teaspoonsPerTablespoon = 3m<teaspoon/tablespoon>
        let tablespoonsPerFluidOunceUs = 2m<tablespoon/fluidOunce_us>
        let tablespoonsPerShot = 3m<tablespoon/shot>
        let fluidOuncePerGillUs = 4m<fluidOunce_us/gill_us>
        let gillsPerCupUs = 2m<gill_us/cup>
        let cupsPerPintUs = 2m<cup/pint_us>
        let pintsPerQuartUs = 2m<pint_us/quart_us>
        let quartsPerPottleUs = 2m<quart_us/pottle>
        let pottlesPerGallonUs = 2m<pottle/gallon_us>
        let gallonsPerBarrel = 31.5m<gallon_us/barrel>
        let barrelsPerHogshead = 2m<barrel/hogshead>

        let minimsUsToFluidDramsUs (min: decimal<minim_us>) = min / minimPerFluidDramUs
        let fluidDramsUsToMinimsUs (fldr: decimal<fluidDram_us>) = fldr * minimPerFluidDramUs
        let minimsUsToTeaspoons (min: decimal<minim_us>) = min / minimPerTeaspoon
        let teaspoonsToMinimsUs (tsp: decimal<teaspoon>) = tsp * minimPerTeaspoon
        let teaspoonsToTablespoons (tsp: decimal<teaspoon>) = tsp / teaspoonsPerTablespoon
        let tablespoonsToTeaspoons (tbsp: decimal<tablespoon>) = tbsp * teaspoonsPerTablespoon
        let fluidOuncesUsToTablespoons (flozUs: decimal<fluidOunce_us>) = flozUs * tablespoonsPerFluidOunceUs
        let tablespoonsToFluidOuncesUs (tbsp: decimal<tablespoon>) = tbsp / tablespoonsPerFluidOunceUs
        let tablespoonsToShots (tbsp: decimal<tablespoon>) = tbsp / tablespoonsPerShot
        let shotsToTablespoons (shot: decimal<shot>) = shot * tablespoonsPerShot
        let fluidOuncesUsToGillsUs (flOzUs: decimal<fluidOunce_us>) = flOzUs / fluidOuncePerGillUs
        let gillsUsToFluidOuncesUs (gill_us: decimal<gill_us>) = gill_us * fluidOuncePerGillUs
        let cupsToGillsUs (cup: decimal<cup>) = cup * gillsPerCupUs
        let gillsUsToCups (gill_us: decimal<gill_us>) = gill_us / gillsPerCupUs
        let cupsToPintsUs (cup: decimal<cup>) = cup / cupsPerPintUs
        let pintsUsToCups (pint: decimal<pint_us>) = pint * cupsPerPintUs
        let pintsUsToQuartsUs (pint: decimal<pint_us>) = pint / pintsPerQuartUs
        let quartsUsToPintsUs (qt: decimal<quart_us>) = qt * pintsPerQuartUs
        let pottlesToQuartsUs (pot: decimal<pottle>) = pot * quartsPerPottleUs
        let quartsUsToPottles (qt: decimal<quart_us>) = qt / quartsPerPottleUs
        let pottlesToGallonsUs (pot: decimal<pottle>) = pot / pottlesPerGallonUs
        let gallonsUsToPottles (gal: decimal<gallon_us>) = gal * pottlesPerGallonUs
        let gallonsUsToBarrels (gal: decimal<gallon_us>) = gal / gallonsPerBarrel
        let barrelsToGallonsUs (bbl: decimal<barrel>) = bbl * gallonsPerBarrel
        let barrelsToHogsheads (bbl: decimal<barrel>) = bbl / barrelsPerHogshead
        let hogsheadToBarrels (hogs: decimal<hogshead>) = hogs * barrelsPerHogshead

        let cubicInchesToGallonsUs (cuin: decimal<cubicInch>) = cuin / cubicInchesPerGallonUs
        let gallonsUsToCubicInches (gal: decimal<gallon_us>) = gal * cubicInchesPerGallonUs
        let gallonsUsToLiters (gal: decimal<gallon_us>) = gal * litersPerUSGallon
        let litersToGallonsUs (l: decimal<liter>) = l / litersPerUSGallon

        //UK Volume
        let minimUkPerFluidDrachmUk = 60m<minim_uk/fluidDrachm_uk>
        let fluidDrachmPerFlOzUk = 8m<fluidDrachm_uk/fluidOunce_uk>
        let fluidOuncePerGillUk = 5m<fluidOunce_uk/gill_uk>
        let gillsPerPintUk = 4m<gill_uk/pint_uk>
        let pintsPerQuartUk = 2m<pint_uk/quart_uk>
        let quartsPerGallonUk = 4m<quart_uk/gallon_uk>
        let gallonsPerPeckUk = 2m<gallon_uk/peck>
        let pecksPerBushelUk = 4m<peck/bushel>
        let bushelsPerQuarterUk = 8m<bushel/quarter_vl>
        let minimsUkToFluidDrachmsUk (min: decimal<minim_uk>) = min / minimUkPerFluidDrachmUk
        let fluidDrachmsUkToMinimsUk (fldr: decimal<fluidDrachm_uk>) = fldr * minimUkPerFluidDrachmUk
        let fluidDrachmsUkToFluidOuncesUk (flDrUk: decimal<fluidDrachm_uk>) = flDrUk / fluidDrachmPerFlOzUk
        let fluidOuncesUkToFluidDrachmsUk (flOzUk: decimal<fluidOunce_uk>) = flOzUk * fluidDrachmPerFlOzUk
        let fluidOuncesUkToGillsUk (floz: decimal<fluidOunce_uk>) = floz / fluidOuncePerGillUk
        let gillsUkToFluidOuncesUk (gil: decimal<gill_uk>) = gil * fluidOuncePerGillUk
        let pintsUkToGillsUk (pt: decimal<pint_uk>) = pt * gillsPerPintUk
        let gillsUkToPintsUk (gil: decimal<gill_uk>) = gil / gillsPerPintUk
        let pintsUkToQuartsUk (pt: decimal<pint_uk>) = pt / pintsPerQuartUk
        let quartsUkToPintsUk (qt: decimal<quart_uk>) = qt * pintsPerQuartUk
        let quartsUkToGallonsUk (qt: decimal<quart_uk>) = qt / quartsPerGallonUk
        let gallonsUkToQuartsUk (gal: decimal<gallon_uk>) = gal * quartsPerGallonUk
        let pecksUkToGallonsUk (pk: decimal<peck>) = pk * gallonsPerPeckUk
        let gallonsUkToPecks (gal: decimal<gallon_uk>) = gal / gallonsPerPeckUk
        let pecksToBushels (gal: decimal<peck>) = gal / pecksPerBushelUk
        let bushelToPecks (bh: decimal<bushel>) = bh * pecksPerBushelUk
        let bushelsToQuarters (bh: decimal<bushel>) = bh / bushelsPerQuarterUk
        let quartersUkToBushels (qtr: decimal<quarter_vl>) = qtr * bushelsPerQuarterUk
        let gallonsUkToLiters (gal: decimal<gallon_uk>) = gal * litersPerUKGallon
        let litersToGallonsUk (l: decimal<liter>) = l / litersPerUKGallon

        let gallonsUkToGallonsUs (gal: decimal<gallon_uk>) = gal * usGallonsPerUkGallon
        let gallonsUsToGallonsUk (gal: decimal<gallon_us>) = gal / usGallonsPerUkGallon

        //Metric
        let millilitersPerLiter: decimal<milliliter/liter> = 1000m<milliliter/liter>

        let litersToMilliliters (l: decimal<liter>) = l * millilitersPerLiter
        let millilitersToLiters (ml: decimal<milliliter>) = ml / millilitersPerLiter

        //Cross-conversions
        let usGallonsToUkGallons (usGallon: decimal<gallon_us>) = usGallon / usGallonsPerUkGallon

        let millilitersToGallonsUs = millilitersToLiters >> litersToGallonsUs
        let millilitersToPottles = millilitersToGallonsUs >> gallonsUsToPottles
        let millilitersToQuartsUs = millilitersToPottles >> pottlesToQuartsUs
        let millilitersToPintsUs = millilitersToQuartsUs >> quartsUsToPintsUs
        let millilitersToCups = millilitersToPintsUs >> pintsUsToCups
        let millilitersToGillsUs = millilitersToCups >> cupsToGillsUs
        let millilitersToFluidOuncesUs = millilitersToGillsUs >> gillsUsToFluidOuncesUs
        let millilitersToTablespoons = millilitersToFluidOuncesUs >> fluidOuncesUsToTablespoons
        let millilitersToShots = millilitersToTablespoons >> tablespoonsToShots
        let millilitersToTeaspoons = millilitersToTablespoons >> tablespoonsToTeaspoons
        let millilitersToMinimsUs = millilitersToTeaspoons >> teaspoonsToMinimsUs
        let millilitersToFluidDramsUs = millilitersToMinimsUs >> minimsUsToFluidDramsUs
        let millilitersToBarrels = millilitersToGallonsUs >> gallonsUsToBarrels
        let millilitersToHogshead = millilitersToBarrels >> barrelsToHogsheads
        let millilitersToCubicInches = millilitersToGallonsUs >> gallonsUsToCubicInches
        let millilitersToGallonsUk = millilitersToLiters >> litersToGallonsUk
        let millilitersToPecks = millilitersToGallonsUk >> gallonsUkToPecks
        let millilitersToQuartsUk = millilitersToGallonsUk >> gallonsUkToQuartsUk
        let millilitersToPintsUk = millilitersToQuartsUk >> quartsUkToPintsUk
        let millilitersToGillsUk = millilitersToPintsUk >> pintsUkToGillsUk
        let millilitersToBushelsUk = millilitersToPecks >> pecksToBushels
        let millilitersToQuartersUk = millilitersToBushelsUk >> bushelsToQuarters 
        let millilitersToFluidOuncesUk = millilitersToGillsUk >> gillsUkToFluidOuncesUk
        let millilitersToFluidDrachmsUk = millilitersToFluidOuncesUk >> fluidOuncesUkToFluidDrachmsUk
        let millilitersToMinimsUk = millilitersToFluidDrachmsUk >> fluidDrachmsUkToMinimsUk
        
        let litersToBarrels = litersToGallonsUs >> gallonsUsToBarrels
        let litersToCubicInches = litersToGallonsUs >> gallonsUsToCubicInches
        let litersToPottles = litersToGallonsUs >> gallonsUsToPottles
        let litersToQuartsUs = litersToPottles >> pottlesToQuartsUs
        let litersToPintsUs = litersToQuartsUs >> quartsUsToPintsUs
        let litersToCups = litersToPintsUs >> pintsUsToCups
        let litersToGillsUs = litersToCups >> cupsToGillsUs
        let litersToFluidOuncesUs = litersToGillsUs >> gillsUsToFluidOuncesUs
        let litersToTablespoons = litersToFluidOuncesUs >> fluidOuncesUsToTablespoons
        let litersToShots = litersToTablespoons >> tablespoonsToShots
        let litersToTeaspoons = litersToTablespoons >> tablespoonsToTeaspoons
        let litersToMinimsUs = litersToTeaspoons >> teaspoonsToMinimsUs
        let litersToHogshead = litersToBarrels >> barrelsToHogsheads
        let litersToPecks = litersToGallonsUk >> gallonsUkToPecks
        let litersToBushels = litersToPecks >> pecksToBushels
        let litersToQuartersUk = litersToBushels >> bushelsToQuarters
        let litersToQuartsUk = litersToGallonsUk >> gallonsUkToQuartsUk
        let litersToPintsUk = litersToQuartsUk >> quartsUkToPintsUk
        let litersToGillsUk = litersToPintsUk >> pintsUkToGillsUk
        let litersToFluidOuncesUk = litersToGillsUk >> gillsUkToFluidOuncesUk
        let litersToFluidDrachmsUk = litersToFluidOuncesUk >> fluidOuncesUkToFluidDrachmsUk
        let litersToMinimsUk = litersToFluidDrachmsUk >> fluidDrachmsUkToMinimsUk

        let minimsUsToTablespoons = minimsUsToTeaspoons >> teaspoonsToTablespoons
        let minimsUsToFluidOuncesUs = minimsUsToTablespoons >> tablespoonsToFluidOuncesUs
        let minimsUsToShots = minimsUsToTablespoons >> tablespoonsToShots
        let minimsUsToGillsUs = minimsUsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let minimsUsToCups = minimsUsToGillsUs >> gillsUsToCups
        let minimsUsToPintsUs = minimsUsToCups >> cupsToPintsUs
        let minimsUsToQuartsUs = minimsUsToPintsUs >> pintsUsToQuartsUs
        let minimsUsToPottles = minimsUsToQuartsUs >> quartsUsToPottles
        let minimsUsToGallonsUs = minimsUsToPottles >> pottlesToGallonsUs
        let minimsUsToCubicInches = minimsUsToGallonsUs >> gallonsUsToCubicInches
        let minimsUsToBarrels = minimsUsToGallonsUs >> gallonsUsToBarrels
        let minimsUsToLiters = minimsUsToGallonsUs >> gallonsUsToLiters
        let minimsUsToMilliliters = minimsUsToLiters >> litersToMilliliters
        let minimsUsToGallonsUk = minimsUsToGallonsUs >> gallonsUsToGallonsUk
        let minimsUsToPecks = minimsUsToGallonsUk >> gallonsUkToPecks
        let minimsUsToBushels = minimsUsToPecks >> pecksToBushels
        let minimsUsToQuartsUk = minimsUsToGallonsUk >> gallonsUkToQuartsUk
        let minimsUsToPintsUk = minimsUsToQuartsUk >> quartsUkToPintsUk
        let minimsUsToGillsUk = minimsUsToPintsUk >> pintsUkToGillsUk
        let minimsUsToFluidOuncesUk = minimsUsToGillsUk >> gillsUkToFluidOuncesUk
        let minimsUsToFluidDrachmsUk = minimsUsToFluidOuncesUk >> fluidOuncesUkToFluidDrachmsUk
        let minimsUsToMinimsUk = minimsUsToFluidDrachmsUk >> fluidDrachmsUkToMinimsUk
        let minimsUsToHogshead = minimsUsToBarrels >> barrelsToHogsheads
        let minimsUsToQuartersUk = minimsUsToBushels >> bushelsToQuarters

        let minimsUkToFluidOuncesUk = minimsUkToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let minimsUkToGillsUk = minimsUkToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let minimsUkToPintsUk = minimsUkToGillsUk >> gillsUkToPintsUk
        let minimsUkToQuartsUk = minimsUkToPintsUk >> pintsUkToQuartsUk
        let minimsUkToGallonsUk = minimsUkToQuartsUk >> quartsUkToGallonsUk
        let minimsUkToPecks = minimsUkToGallonsUk >> gallonsUkToPecks
        let minimsUkToGallonsUs = minimsUkToGallonsUk >> gallonsUkToGallonsUs
        let minimsUkToCubicInches = minimsUkToGallonsUs >> gallonsUsToCubicInches
        let minimsUkToBarrels = minimsUkToGallonsUs >> gallonsUsToBarrels
        let minimsUkToLiters = minimsUkToGallonsUs >> gallonsUsToLiters
        let minimsUkToPottles = minimsUkToGallonsUs >> gallonsUsToPottles
        let minimsUkToQuartsUs = minimsUkToPottles >> pottlesToQuartsUs
        let minimsUkToPintsUs = minimsUkToQuartsUs >> quartsUsToPintsUs
        let minimsUkToCups = minimsUkToPintsUs >> pintsUsToCups
        let minimsUkToGillsUs = minimsUkToCups >> cupsToGillsUs
        let minimsUkToFluidOuncesUs = minimsUkToGillsUs >> gillsUsToFluidOuncesUs
        let minimsUkToTablespoons = minimsUkToFluidOuncesUs >> fluidOuncesUsToTablespoons
        let minimsUkToShots = minimsUkToTablespoons >> tablespoonsToShots
        let minimsUkToTeaspoons = minimsUkToTablespoons >> tablespoonsToTeaspoons
        let minimsUkToMinimsUs = minimsUkToTeaspoons >> teaspoonsToMinimsUs
        let minimsUkToFluidDramsUs = minimsUkToMinimsUs >> minimsUsToFluidDramsUs
        let minimsUkToHogshead = minimsUkToBarrels >> barrelsToHogsheads
        let minimsUkToBushels = minimsUkToPecks >> pecksToBushels
        let minimsUkToQuarters = minimsUkToBushels >> bushelsToQuarters
        let minimsUkToMilliliters = minimsUkToLiters >> litersToMilliliters

        let fluidDramsUsToTeaspoons = fluidDramsUsToMinimsUs >> minimsUsToTeaspoons
        let fluidDramsUsToTablespoons = fluidDramsUsToTeaspoons >> teaspoonsToTablespoons
        let fluidDramsUsToFluidOuncesUs = fluidDramsUsToTablespoons >> tablespoonsToFluidOuncesUs
        let fluidDramsUsToShots = fluidDramsUsToTablespoons >> tablespoonsToShots
        let fluidDramsUsToGillsUs = fluidDramsUsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let fluidDramsUsToCups = fluidDramsUsToGillsUs >> gillsUsToCups
        let fluidDramsUsToPintsUs = fluidDramsUsToCups >> cupsToPintsUs
        let fluidDramsUsToQuartsUs = fluidDramsUsToPintsUs >> pintsUsToQuartsUs
        let fluidDramsUsToPottles = fluidDramsUsToQuartsUs >> quartsUsToPottles
        let fluidDramsUsToGallonsUs = fluidDramsUsToPottles >> pottlesToGallonsUs
        let fluidDramsUsToBarrels = fluidDramsUsToGallonsUs >> gallonsUsToBarrels
        let fluidDramsUsToHogshead = fluidDramsUsToBarrels >> barrelsToHogsheads
        let fluidDramsUsToCubicInch = fluidDramsUsToGallonsUs >> gallonsUsToCubicInches
        let fluidDramsUsToMinimsUk = fluidDramsUsToMinimsUs >> minimsUsToMinimsUk
        let fluidDramsUsToFluidDrachmsUk = fluidDramsUsToMinimsUk >> minimsUkToFluidDrachmsUk
        let fluidDramsUsToFluidOuncesUk = fluidDramsUsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let fluidDramsUsToGillsUk = fluidDramsUsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let fluidDramsUsToPintsUk = fluidDramsUsToGillsUk >> gillsUkToPintsUk
        let fluidDramsUsToQuartsUk = fluidDramsUsToPintsUk >> pintsUkToQuartsUk
        let fluidDramsUsToGallonsUk = fluidDramsUsToQuartsUk >> quartsUkToGallonsUk
        let fluidDramsUsToPecks = fluidDramsUsToGallonsUk >> gallonsUkToPecks
        let fluidDramsUsToBushels = fluidDramsUsToPecks >> pecksToBushels
        let fluidDramsUsToQuarters = fluidDramsUsToBushels >> bushelsToQuarters

        let fluidDrachmsUkToMinimsUs = fluidDrachmsUkToMinimsUk >> minimsUkToMinimsUs
        let fluidDrachmsUkToFluidDramsUs = fluidDrachmsUkToMinimsUs >> minimsUsToFluidDramsUs
        let fluidDrachmsUkToTeaspoons = fluidDrachmsUkToFluidDramsUs >> fluidDramsUsToTeaspoons
        let fluidDrachmsUkToTablespoons = fluidDrachmsUkToTeaspoons >> teaspoonsToTablespoons
        let fluidDrachmsUkToFluidOuncesUs = fluidDrachmsUkToTablespoons >> tablespoonsToFluidOuncesUs
        let fluidDrachmsUkToShots = fluidDrachmsUkToTablespoons >> tablespoonsToShots
        let fluidDrachmsUkToGillsUs = fluidDrachmsUkToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let fluidDrachmsUkToCups = fluidDrachmsUkToGillsUs >> gillsUsToCups
        let fluidDrachmsUkToPintsUs = fluidDrachmsUkToCups >> cupsToPintsUs
        let fluidDrachmsUkToQuartsUs = fluidDrachmsUkToPintsUs >> pintsUsToQuartsUs
        let fluidDrachmsUkToPottles = fluidDrachmsUkToQuartsUs >> quartsUsToPottles
        let fluidDrachmsUkToGallonsUs = fluidDrachmsUkToPottles >> pottlesToGallonsUs
        let fluidDrachmsUkToBarrels = fluidDrachmsUkToGallonsUs >> gallonsUsToBarrels
        let fluidDrachmsUkToHogshead = fluidDrachmsUkToBarrels >> barrelsToHogsheads
        let fluidDrachmsUkToCubicInches = fluidDrachmsUkToGallonsUs >> gallonsUsToCubicInches
        let fluidDrachmsUkToGillsUk = fluidDrachmsUkToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let fluidDrachmsUkToPintsUk = fluidDrachmsUkToGillsUk >> gillsUkToPintsUk
        let fluidDrachmsUkToQuartsUk = fluidDrachmsUkToPintsUk >> pintsUkToQuartsUk
        let fluidDrachmsUkToGallonsUk = fluidDrachmsUkToQuartsUk >> quartsUkToGallonsUk
        let fluidDrachmsUkToPecks = fluidDrachmsUkToGallonsUk >> gallonsUkToPecks
        let fluidDrachmsUkToBushels = fluidDrachmsUkToPecks >> pecksToBushels
        let fluidDrachmsUkToQuarters = fluidDrachmsUkToBushels >> bushelsToQuarters

        let teaspoonsToFluidDramsUs = teaspoonsToMinimsUs >> minimsUsToFluidDramsUs
        let teaspoonsToFluidOuncesUs = teaspoonsToTablespoons >> tablespoonsToFluidOuncesUs
        let teaspoonsToShots = teaspoonsToTablespoons >> tablespoonsToShots
        let teaspoonsToGillsUs = teaspoonsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let teaspoonsToCups = teaspoonsToGillsUs >> gillsUsToCups
        let teaspoonsToPintsUs = teaspoonsToCups >> cupsToPintsUs
        let teaspoonsToQuartsUs = teaspoonsToPintsUs >> pintsUsToQuartsUs
        let teaspoonsToPottles = teaspoonsToQuartsUs >> quartsUsToPottles
        let teaspoonsToGallonsUs = teaspoonsToPottles >> pottlesToGallonsUs
        let teaspoonsToBarrels = teaspoonsToGallonsUs >> gallonsUsToBarrels
        let teaspoonsToHogshead = teaspoonsToBarrels >> barrelsToHogsheads
        let teaspoonsToCubicInches = teaspoonsToGallonsUs >> gallonsUsToCubicInches
        let teaspoonsToMinimsUk = teaspoonsToMinimsUs >> minimsUsToMinimsUk
        let teaspoonsToFluidDrachmsUk = teaspoonsToMinimsUk >> minimsUkToFluidDrachmsUk
        let teaspoonsToFluidOuncesUk = teaspoonsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let teaspoonsToGillsUk = teaspoonsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let teaspoonsToPintsUk = teaspoonsToGillsUk >> gillsUkToPintsUk
        let teaspoonsToQuartsUk = teaspoonsToPintsUk >> pintsUkToQuartsUk
        let teaspoonsToGallonsUk = teaspoonsToQuartsUk >> quartsUkToGallonsUk
        let teaspoonsToPecks = teaspoonsToGallonsUk >> gallonsUkToPecks
        let teaspoonsToBushels = teaspoonsToPecks >> pecksToBushels
        let teaspoonsToQuarters = teaspoonsToBushels >> bushelsToQuarters

        let tablespoonsToMinimsUs = tablespoonsToTeaspoons >> teaspoonsToMinimsUs
        let tablespoonsToFluidDramUs = tablespoonsToMinimsUs >> minimsUsToFluidDramsUs
        let tablespoonsToGillsUs = tablespoonsToFluidDramUs >> fluidDramsUsToGillsUs
        let tablespoonsToCups = tablespoonsToGillsUs >> gillsUsToCups
        let tablespoonsToPintsUs = tablespoonsToCups >> cupsToPintsUs
        let tablespoonsToQuartsUs = tablespoonsToPintsUs >> pintsUsToQuartsUs
        let tablespoonsToPottles = tablespoonsToQuartsUs >> quartsUsToPottles
        let tablespoonsToGallonsUs = tablespoonsToPottles >> pottlesToGallonsUs
        let tablespoonsToBarrels = tablespoonsToGallonsUs >> gallonsUsToBarrels
        let tablespoonsToHogsheads = tablespoonsToBarrels >> barrelsToHogsheads
        let tablespoonsToCubicInches = tablespoonsToGallonsUs >> gallonsUsToCubicInches
        let tablespoonsToMinimsUk = tablespoonsToMinimsUs >> minimsUsToMinimsUk
        let tablespoonsToFluidDrachmsUk = tablespoonsToMinimsUk >> minimsUkToFluidDrachmsUk
        let tablespoonsToFluidOuncesUk = tablespoonsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let tablespoonsToGillsUk = tablespoonsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let tablespoonsToPintsUk = tablespoonsToGillsUk >> gillsUkToPintsUk
        let tablespoonsToQuartsUk = tablespoonsToPintsUk >> pintsUkToQuartsUk
        let tablespoonsToGallonsUk = tablespoonsToQuartsUk >> quartsUkToGallonsUk
        let tablespoonsToPecks = tablespoonsToGallonsUk >> gallonsUkToPecks
        let tablespoonsToBushels = tablespoonsToPecks >> pecksToBushels
        let tablespoonsToQuarters = tablespoonsToBushels >> bushelsToQuarters

        let fluidOuncesUsToMinimsUs = fluidOuncesUsToTablespoons >> tablespoonsToMinimsUs
        let fluidOuncesUsToFluidDramsUs = fluidOuncesUsToMinimsUs >> minimsUsToFluidDramsUs
        let fluidOuncesUsToTeaspoons = fluidOuncesUsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let fluidOuncesUsToShots = fluidOuncesUsToTeaspoons >> teaspoonsToShots
        let fluidOuncesUsToCups = fluidOuncesUsToTablespoons >> tablespoonsToCups
        let fluidOuncesUsToPintsUs = fluidOuncesUsToCups >> cupsToPintsUs
        let fluidOuncesUsToQuartsUs = fluidOuncesUsToPintsUs >> pintsUsToQuartsUs
        let fluidOuncesUsToPottles = fluidOuncesUsToQuartsUs >> quartsUsToPottles
        let fluidOuncesUsToGallonsUs = fluidOuncesUsToPottles >> pottlesToGallonsUs
        let fluidOuncesUsToBarrels = fluidOuncesUsToGallonsUs >> gallonsUsToBarrels
        let fluidOuncesUsToHogsheads = fluidOuncesUsToBarrels >> barrelsToHogsheads
        let fluidOuncesUsToCubicInches = fluidOuncesUsToGallonsUs >> gallonsUsToCubicInches
        let fluidOuncesUsToMinimsUk = fluidOuncesUsToMinimsUs >> minimsUsToMinimsUk
        let fluidOuncesUsToFluidDrachmsUk = fluidOuncesUsToMinimsUk >> minimsUkToFluidDrachmsUk
        let fluidOuncesUsToFluidOuncesUk = fluidOuncesUsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let fluidOuncesUsToGillsUk = fluidOuncesUsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let fluidOuncesUsToPintsUk = fluidOuncesUsToGillsUk >> gillsUkToPintsUk
        let fluidOuncesUsToQuartsUk = fluidOuncesUsToPintsUk >> pintsUkToQuartsUk
        let fluidOuncesUsToGallonsUk = fluidOuncesUsToQuartsUk >> quartsUkToGallonsUk
        let fluidOuncesUsToPecks = fluidOuncesUsToGallonsUk >> gallonsUkToPecks
        let fluidOuncesUsToBushels = fluidOuncesUsToPecks >> pecksToBushels
        let fluidOuncesUsToQuarters = fluidOuncesUsToBushels >> bushelsToQuarters

        let fluidOuncesUkToMinimsUs = fluidOuncesUkToFluidDrachmsUk >> fluidDrachmsUkToMinimsUs
        let fluidOuncesUkToFluidOuncesUs = fluidOuncesUkToMinimsUs >> minimsUsToFluidOuncesUs
        let fluidOuncesUkToFluidDramsUs = fluidOuncesUkToMinimsUs >> minimsUsToFluidDramsUs
        let fluidOuncesUkToTeaspoons = fluidOuncesUkToFluidDramsUs >> fluidDramsUsToTeaspoons
        let fluidOuncesUkToTablespoons = fluidOuncesUkToTeaspoons >> teaspoonsToTablespoons
        let fluidOuncesUkToShots = fluidOuncesUkToTablespoons >> tablespoonsToShots
        let fluidOuncesUkToGillsUs = fluidOuncesUkToTablespoons >> tablespoonsToGillsUs
        let fluidOuncesUkToCups = fluidOuncesUkToGillsUs >> gillsUsToCups
        let fluidOuncesUkToPintsUs = fluidOuncesUkToCups >> cupsToPintsUs
        let fluidOuncesUkToQuartsUs = fluidOuncesUkToPintsUs >> pintsUsToQuartsUs
        let fluidOuncesUkToPottles = fluidOuncesUkToQuartsUs >> quartsUsToPottles
        let fluidOuncesUkToGallonsUs = fluidOuncesUkToPottles >> pottlesToGallonsUs
        let fluidOuncesUkToBarrels = fluidOuncesUkToGallonsUs >> gallonsUsToBarrels
        let fluidOuncesUkToHogsheads = fluidOuncesUkToBarrels >> barrelsToHogsheads
        let fluidOuncesUkToCubicInches = fluidOuncesUkToGallonsUs >> gallonsUsToCubicInches
        let fluidOuncesUkToMinimsUk = fluidOuncesUkToFluidDrachmsUk >> fluidDrachmsUkToMinimsUk
        let fluidOuncesUkToPintsUk = fluidOuncesUkToGillsUk >> gillsUkToPintsUk
        let fluidOuncesUkToQuartsUk = fluidOuncesUkToPintsUk >> pintsUkToQuartsUk
        let fluidOuncesUkToGallonsUk = fluidOuncesUkToQuartsUk >> quartsUkToGallonsUk
        let fluidOuncesUkToPecks = fluidOuncesUkToGallonsUk >> gallonsUkToPecks
        let fluidOuncesUkToBushels = fluidOuncesUkToPecks >> pecksToBushels
        let fluidOuncesUkToQuarters = fluidOuncesUkToBushels >> bushelsToQuarters

        let gillsUsToMinimsUs = gillsUsToFluidOuncesUs >> fluidOuncesUsToMinimsUs
        let gillsUsToFluidDramsUs = gillsUsToMinimsUs >> minimsUsToFluidDramsUs
        let gillsUsToTeaspoons = gillsUsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let gillsUsToTablespoons = gillsUsToTeaspoons >> teaspoonsToTablespoons
        let gillsUsToShots = gillsUsToTablespoons >> tablespoonsToShots
        let gillsUsToPintsUs = gillsUsToCups >> cupsToPintsUs
        let gillsUsToQuartsUs = gillsUsToPintsUs >> pintsUsToQuartsUs
        let gillsUsToPottles = gillsUsToQuartsUs >> quartsUsToPottles
        let gillsUsToGallonsUs = gillsUsToPottles >> pottlesToGallonsUs
        let gillsUsToBarrels = gillsUsToGallonsUs >> gallonsUsToBarrels
        let gillsUsToHogsheads = gillsUsToBarrels >> barrelsToHogsheads
        let gillsUsToCubicInches = gillsUsToGallonsUs >> gallonsUsToCubicInches
        let gillsUsToMinimsUk = gillsUsToMinimsUs >> minimsUsToMinimsUk
        let gillsUsToFluidDrachmsUk = gillsUsToMinimsUk >> minimsUkToFluidDrachmsUk
        let gillsUsToFluidOuncesUk = gillsUsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let gillsUsToGillsUk = gillsUsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let gillsUsToPintsUk = gillsUsToGillsUk >> gillsUkToPintsUk
        let gillsUsToQuartsUk = gillsUsToPintsUk >> pintsUkToQuartsUk
        let gillsUsToGallonsUk = gillsUsToQuartsUk >> quartsUkToGallonsUk
        let gillsUsToPecks = gillsUsToGallonsUk >> gallonsUkToPecks
        let gillsUsToBushels = gillsUsToPecks >> pecksToBushels
        let gillsUsToQuarters = gillsUsToBushels >> bushelsToQuarters

        let gillsUkToMinimsUs = gillsUkToFluidOuncesUk >> fluidOuncesUkToMinimsUs
        let gillsUkToFluidDramsUs = gillsUkToMinimsUs >> minimsUsToFluidDramsUs
        let gillsUkToTeaspoons = gillsUkToFluidDramsUs >> fluidDramsUsToTeaspoons
        let gillsUkToTablespoons = gillsUkToTeaspoons >> teaspoonsToTablespoons
        let gillsUkToFluidOuncesUs = gillsUkToTablespoons >> tablespoonsToFluidOuncesUs
        let gillsUkToShots = gillsUkToTablespoons >> tablespoonsToShots
        let gillsUkToGillsUs = gillsUkToTeaspoons >> teaspoonsToGillsUs
        let gillsUkToCups = gillsUkToGillsUs >> gillsUsToCups
        let gillsUkToPintsUs = gillsUkToCups >> cupsToPintsUs
        let gillsUkToQuartsUs = gillsUkToPintsUs >> pintsUsToQuartsUs
        let gillsUkToPottles = gillsUkToQuartsUs >> quartsUsToPottles
        let gillsUkToGallonsUs = gillsUkToPottles >> pottlesToGallonsUs
        let gillsUkToBarrels = gillsUkToGallonsUs >> gallonsUsToBarrels
        let gillsUkToHogsheads = gillsUkToBarrels >> barrelsToHogsheads
        let gillsUkToCubicInches = gillsUkToGallonsUs >> gallonsUsToCubicInches
        let gillsUkToMinimsUk = gillsUkToFluidOuncesUk >> fluidOuncesUkToMinimsUk
        let gillsUkToFluidDrachmsUk = gillsUkToMinimsUk >> minimsUkToFluidDrachmsUk
        let gillsUkToQuartsUk = gillsUkToPintsUk >> pintsUkToQuartsUk
        let gillsUkToGallonsUk = gillsUkToQuartsUk >> quartsUkToGallonsUk
        let gillsUkToPecks = gillsUkToGallonsUk >> gallonsUkToPecks
        let gillsUkToBushels = gillsUkToPecks >> pecksToBushels
        let gillsUkToQuarters = gillsUkToBushels >> bushelsToQuarters

        let shotsToMinimsUs = shotsToTablespoons >> tablespoonsToMinimsUs
        let shotsToFluidDramsUs = shotsToMinimsUs >> minimsUsToFluidDramsUs
        let shotsToTeaspoons = shotsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let shotsToFluidOunceUs = shotsToTablespoons >> tablespoonsToFluidOuncesUs
        let shotsToGillsUs = shotsToFluidOunceUs >> fluidOuncesUsToGillsUs
        let shotsToCups = shotsToGillsUs >> gillsUsToCups
        let shotsToPintsUs = shotsToCups >> cupsToPintsUs
        let shotsToQuartsUs = shotsToPintsUs >> pintsUsToQuartsUs
        let shotsToPottles = shotsToQuartsUs >> quartsUsToPottles
        let shotsToGallonsUs = shotsToPottles >> pottlesToGallonsUs
        let shotsToBarrels = shotsToGallonsUs >> gallonsUsToBarrels
        let shotsToHogsheads = shotsToBarrels >> barrelsToHogsheads
        let shotsToCubicInches = shotsToGallonsUs >> gallonsUsToCubicInches
        let shotsToMinimsUk = shotsToMinimsUs >> minimsUsToMinimsUk
        let shotsToFluidDrachmsUk = shotsToMinimsUk >> minimsUkToFluidDrachmsUk
        let shotsToFluidOuncesUk = shotsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let shotsToGillsUk = shotsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let shotsToPintsUk = shotsToGillsUk >> gillsUkToPintsUk
        let shotsToQuartsUk = shotsToPintsUk >> pintsUkToQuartsUk
        let shotsToGallonsUk = shotsToQuartsUk >> quartsUkToGallonsUk
        let shotsToPecks = shotsToGallonsUk >> gallonsUkToPecks
        let shotsToBushels = shotsToPecks >> pecksToBushels
        let shotsToQuarters = shotsToBushels >> bushelsToQuarters

        let cupsToMinimsUs = cupsToGillsUs >> gillsUsToMinimsUs
        let cupsToFluidDramsUs = cupsToMinimsUs >> minimsUsToFluidDramsUs
        let cupsToTeaspoons = cupsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let cupsToTablespoons = cupsToTeaspoons >> teaspoonsToTablespoons
        let cupsToFluidOuncesUs = cupsToTablespoons >> tablespoonsToFluidOuncesUs
        let cupsToShots = cupsToFluidOuncesUs >> fluidOuncesUsToShots
        let cupsToQuartsUs = cupsToPintsUs >> pintsUsToQuartsUs
        let cupsToPottles = cupsToQuartsUs >> quartsUsToPottles
        let cupsToGallonsUs = cupsToPottles >> pottlesToGallonsUs
        let cupsToBarrels = cupsToGallonsUs >> gallonsUsToBarrels
        let cupsToHogsheads = cupsToBarrels >> barrelsToHogsheads
        let cupsToCubicInches = cupsToGallonsUs >> gallonsUsToCubicInches
        let cupsToMinimsUk = cupsToMinimsUs >> minimsUsToMinimsUk
        let cupsToFluidDrachmsUk = cupsToMinimsUk >> minimsUkToFluidDrachmsUk
        let cupsToFluidOuncesUk = cupsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let cupsToGillsUk = cupsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let cupsToPintsUk = cupsToGillsUk >> gillsUkToPintsUk
        let cupsToQuartsUk = cupsToPintsUk >> pintsUkToQuartsUk
        let cupsToGallonsUk = cupsToQuartsUk >> quartsUkToGallonsUk
        let cupsToPecks = cupsToGallonsUk >> gallonsUkToPecks
        let cupsToBushels = cupsToPecks >> pecksToBushels
        let cupsToQuarters = cupsToBushels >> bushelsToQuarters

        let pintsUsToMinimsUs = pintsUsToCups >> cupsToMinimsUs
        let pintsUsToFluidDramsUs = pintsUsToMinimsUs >> minimsUsToFluidDramsUs
        let pintsUsToTeaspoons = pintsUsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let pintsUsToTablespoons = pintsUsToTeaspoons >> teaspoonsToTablespoons
        let pintsUsToFluidOuncesUs = pintsUsToTablespoons >> tablespoonsToFluidOuncesUs
        let pintsUsToShots = pintsUsToTablespoons >> tablespoonsToShots
        let pintsUsToGillsUs = pintsUsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let pintsUsToPottles = pintsUsToQuartsUs >> quartsUsToPottles
        let pintsUsToGallonsUs = pintsUsToPottles >> pottlesToGallonsUs
        let pintsUsToBarrels = pintsUsToGallonsUs >> gallonsUsToBarrels
        let pintsUsToHogsheads = pintsUsToBarrels >> barrelsToHogsheads
        let pintsUsToCubicInches = pintsUsToGallonsUs >> gallonsUsToCubicInches
        let pintsUsToMinimsUk = pintsUsToMinimsUs >> minimsUsToMinimsUk
        let pintsUsToFluidDrachmsUk = pintsUsToMinimsUk >> minimsUkToFluidDrachmsUk
        let pintsUsToFluidOuncesUk = pintsUsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let pintsUsToGillsUk = pintsUsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let pintsUsToPintsUk = pintsUsToGillsUk >> gillsUkToPintsUk
        let pintsUsToQuartsUk = pintsUsToPintsUk >> pintsUkToQuartsUk
        let pintsUsToGallonsUk = pintsUsToQuartsUk >> quartsUkToGallonsUk
        let pintsUsToPecks = pintsUsToGallonsUk >> gallonsUkToPecks
        let pintsUsToBushels = pintsUsToPecks >> pecksToBushels
        let pintsUsToQuarters = pintsUsToBushels >> bushelsToQuarters

        let pintsUkToMinimsUs = pintsUkToGillsUk >> gillsUkToMinimsUs
        let pintsUkToFluidDramsUs = pintsUkToMinimsUs >> minimsUsToFluidDramsUs
        let pintsUkToTeaspoons = pintsUkToFluidDramsUs >> fluidDramsUsToTeaspoons
        let pintsUkToTablespoons = pintsUkToTeaspoons >> teaspoonsToTablespoons
        let pintsUkToFluidOuncesUs = pintsUkToTablespoons >> tablespoonsToFluidOuncesUs
        let pintsUkToShots = pintsUkToFluidOuncesUs >> fluidOuncesUsToShots
        let pintsUkToGillsUs = pintsUkToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let pintsUkToCups = pintsUkToGillsUs >> gillsUsToCups
        let pintsUkToPintsUs = pintsUkToCups >> cupsToPintsUs
        let pintsUkToQuartsUs = pintsUkToPintsUs >> pintsUsToQuartsUs
        let pintsUkToPottles = pintsUkToQuartsUs >> quartsUsToPottles
        let pintsUkToGallonsUs = pintsUkToPottles >> pottlesToGallonsUs
        let pintsUkToBarrels = pintsUkToGallonsUs >> gallonsUsToBarrels
        let pintsUkToHogsheads = pintsUkToBarrels >> barrelsToHogsheads
        let pintsUkToCubicInches = pintsUkToGallonsUs >> gallonsUsToCubicInches
        let pintsUkToMinimsUk = pintsUkToGillsUk >> gillsUkToMinimsUk
        let pintsUkToFluidDrachmsUk = pintsUkToMinimsUk >> minimsUkToFluidDrachmsUk
        let pintsUkToFluidOuncesUk = pintsUkToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let pintsUkToGallonsUk = pintsUkToQuartsUk >> quartsUkToGallonsUk
        let pintsUkToPecks = pintsUkToGallonsUk >> gallonsUkToPecks
        let pintsUkToBushels = pintsUkToPecks >> pecksToBushels
        let pintsUkToQuarters = pintsUkToBushels >> bushelsToQuarters

        let quartsUsToMinimsUs = quartsUsToPintsUs >> pintsUsToMinimsUs
        let quartsUsToFluidDramsUs = quartsUsToMinimsUs >> minimsUsToFluidDramsUs
        let quartsUsToTeaspoons = quartsUsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let quartsUsToTablespoons = quartsUsToTeaspoons >> teaspoonsToTablespoons
        let quartsUsToFluidOuncesUs = quartsUsToTablespoons >> tablespoonsToFluidOuncesUs
        let quartsUsToShots = quartsUsToFluidOuncesUs >> fluidOuncesUsToShots
        let quartsUsToGillsUs = quartsUsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let quartsUsToCups = quartsUsToGillsUs >> gillsUsToCups
        let quartsUsToGallonsUs = quartsUsToPottles >> pottlesToGallonsUs
        let quartsUsToBarrels = quartsUsToGallonsUs >> gallonsUsToBarrels
        let quartsUsToHogsheads = quartsUsToBarrels >> barrelsToHogsheads
        let quartsUsToCubicInches = quartsUsToGallonsUs >> gallonsUsToCubicInches
        let quartsUsToMinimsUk = quartsUsToMinimsUs >> minimsUsToMinimsUk
        let quartsUsToFluidDrachmsUk = quartsUsToMinimsUk >> minimsUkToFluidDrachmsUk
        let quartsUsToFluidOuncesUk = quartsUsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let quartsUsToGillsUk = quartsUsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let quartsUsToPintsUk = quartsUsToGillsUk >> gillsUkToPintsUk
        let quartsUsToQuartsUk = quartsUsToPintsUk >> pintsUkToQuartsUk
        let quartsUsToGallonsUk = quartsUsToQuartsUk >> quartsUkToGallonsUk
        let quartsUsToPecks = quartsUsToGallonsUk >> gallonsUkToPecks
        let quartsUsToBushels = quartsUsToPecks >> pecksToBushels
        let quartsUsToQuarters = quartsUsToBushels >> bushelsToQuarters

        let quartsUkToMinimsUs = quartsUkToPintsUk >> pintsUkToMinimsUs
        let quartsUkToFluidDramsUs = quartsUkToMinimsUs >> minimsUsToFluidDramsUs
        let quartsUkToTeaspoons = quartsUkToFluidDramsUs >> fluidDramsUsToTeaspoons
        let quartsUkToTablespoons = quartsUkToTeaspoons >> teaspoonsToTablespoons
        let quartsUkToFluidOuncesUs = quartsUkToTablespoons >> tablespoonsToFluidOuncesUs
        let quartsUkToShots = quartsUkToFluidOuncesUs >> fluidOuncesUsToShots
        let quartsUkToGillsUs = quartsUkToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let quartsUkToCups = quartsUkToGillsUs >> gillsUsToCups
        let quartsUkToPintsUs = quartsUkToCups >> cupsToPintsUs
        let quartsUkToQuartsUs = quartsUkToPintsUs >> pintsUsToQuartsUs
        let quartsUkToPottles = quartsUkToQuartsUs >> quartsUsToPottles
        let quartsUkToGallonsUs = quartsUkToPottles >> pottlesToGallonsUs
        let quartsUkToBarrels = quartsUkToGallonsUs >> gallonsUsToBarrels
        let quartsUkToHogsheads = quartsUkToBarrels >> barrelsToHogsheads
        let quartsUkToCubicInches = quartsUkToGallonsUs >> gallonsUsToCubicInches
        let quartsUkToMinimsUk = quartsUkToPintsUk >> pintsUkToMinimsUk
        let quartsUkToFluidDrachmsUk = quartsUkToMinimsUk >> minimsUkToFluidDrachmsUk
        let quartsUkToFluidOuncesUk = quartsUkToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let quartsUkToGillsUk = quartsUkToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let quartsUkToPecks = quartsUkToGallonsUk >> gallonsUkToPecks
        let quartsUkToBushels = quartsUkToPecks >> pecksToBushels
        let quartsUkToQuarters = quartsUkToBushels >> bushelsToQuarters

        let pottlesToMinimsUs = pottlesToQuartsUs >> quartsUsToMinimsUs
        let pottlesToFluidDramsUs = pottlesToMinimsUs >> minimsUsToFluidDramsUs
        let pottlesToTeaspoons = pottlesToFluidDramsUs >> fluidDramsUsToTeaspoons
        let pottlesToTablespoons = pottlesToTeaspoons >> teaspoonsToTablespoons
        let pottlesToFluidOuncesUs = pottlesToTablespoons >> tablespoonsToFluidOuncesUs
        let pottlesToShots = pottlesToFluidOuncesUs >> fluidOuncesUsToShots
        let pottlesToGillsUs = pottlesToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let pottlesToCups = pottlesToGillsUs >> gillsUsToCups
        let pottlesToPintsUs = pottlesToCups >> cupsToPintsUs
        let pottlesToBarrels = pottlesToGallonsUs >> gallonsUsToBarrels
        let pottlesToHogsheads = pottlesToBarrels >> barrelsToHogsheads
        let pottlesToCubicInches = pottlesToGallonsUs >> gallonsUsToCubicInches
        let pottlesToMinimsUk = pottlesToMinimsUs >> minimsUsToMinimsUk
        let pottlesToFluidDrachmsUk = pottlesToMinimsUk >> minimsUkToFluidDrachmsUk
        let pottlesToFluidOuncesUk = pottlesToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let pottlesToGillsUk = pottlesToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let pottlesToPintsUk = pottlesToGillsUk >> gillsUkToPintsUk
        let pottlesToQuartsUk = pottlesToPintsUk >> pintsUkToQuartsUk
        let pottlesToGallonsUk = pottlesToQuartsUk >> quartsUkToGallonsUk
        let pottlesToPecks = pottlesToGallonsUk >> gallonsUkToPecks
        let pottlesToBushels = pottlesToPecks >> pecksToBushels
        let pottlesToQuarters = pottlesToBushels >> bushelsToQuarters

        let gallonsUsToMinimsUs = gallonsUsToPottles >> pottlesToMinimsUs
        let gallonsUsToFluidDramsUs = gallonsUsToMinimsUs >> minimsUsToFluidDramsUs
        let gallonsUsToTeaspoons = gallonsUsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let gallonsUsToTablespoons = gallonsUsToTeaspoons >> teaspoonsToTablespoons
        let gallonsUsToFluidOuncesUs = gallonsUsToTablespoons >> tablespoonsToFluidOuncesUs
        let gallonsUsToShots = gallonsUsToFluidOuncesUs >> fluidOuncesUsToShots
        let gallonsUsToGillsUs = gallonsUsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let gallonsUsToCups = gallonsUsToGillsUs >> gillsUsToCups
        let gallonsUsToPintsUs = gallonsUsToCups >> cupsToPintsUs
        let gallonsUsToQuartsUs = gallonsUsToPintsUs >> pintsUsToQuartsUs
        let gallonsUsToHogsheads = gallonsUsToBarrels >> barrelsToHogsheads
        let gallonsUsToMinimsUk = gallonsUsToMinimsUs >> minimsUsToMinimsUk
        let gallonsUsToFluidDrachmsUk = gallonsUsToMinimsUk >> minimsUkToFluidDrachmsUk
        let gallonsUsToFluidOuncesUk = gallonsUsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let gallonsUsToGillsUk = gallonsUsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let gallonsUsToPintsUk = gallonsUsToGillsUk >> gillsUkToPintsUk
        let gallonsUsToQuartsUk = gallonsUsToPintsUk >> pintsUkToQuartsUk
        let gallonsUsToPecks = gallonsUsToGallonsUk >> gallonsUkToPecks
        let gallonsUsToBushels = gallonsUsToPecks >> pecksToBushels
        let gallonsUsToQuarters = gallonsUsToBushels >> bushelsToQuarters


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