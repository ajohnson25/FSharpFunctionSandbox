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
        let drachmsToMilligrams x = drachmsToGrams x |> gramsToMilligrams
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

        type WeightUnit = { amount: decimal; unitName: string}

        let packageWeightUnit (x, y) = {amount = x; unitName = y}
        let removeUnit (x:decimal<_>) = decimal x

        let convertWeightVerified ((x:decimal),fromUnit,toUnit) =
            match (fromUnit,toUnit) with
            | ("grains","drachms") -> {amount = grainsToDrachms(x*1m<grain>) |> removeUnit; unitName = "drachms"}
            | ("grains","ounces") -> {amount = grainsToOunces(x*1m<grain>) |> removeUnit; unitName = "ounces"}
            | ("grains","pounds") -> {amount = grainsToPounds(x*1m<grain>) |> removeUnit; unitName = "pounds"}
            | ("grains","stones") -> {amount = grainsToStones(x*1m<grain>) |> removeUnit; unitName = "stones"}
            | ("grains","quartersLng") -> {amount = grainsToQuartersLng(x*1m<grain>) |> removeUnit; unitName = "quartersLng"}
            | ("grains","quartersShrt") -> {amount = grainsToQuartersShrt(x*1m<grain>) |> removeUnit; unitName = "quartersShrt"}
            | ("grains","hundredweightsLng") -> {amount = grainsToHundredweightsLng(x*1m<grain>) |> removeUnit; unitName = "hundredweightsLng"}
            | ("grains","hundredweightsShrt") -> {amount = grainsToHundredweightsShrt(x*1m<grain>) |> removeUnit; unitName = "hundredweightsShrt"}
            | ("grains","tonsLng") -> {amount = grainsToTonsLng(x*1m<grain>) |> removeUnit; unitName = "tonsLng"}
            | ("grains","tonsShrt") -> {amount = grainsToTonsShrt(x*1m<grain>) |> removeUnit; unitName = "tonsShrt"}
            | ("grains","tonsMetric") -> {amount = grainsToTonsMetric(x*1m<grain>) |> removeUnit; unitName = "tonsMetric"}
            | ("grains","milligrams") -> {amount = grainsToMilligrams(x*1m<grain>) |> removeUnit; unitName = "milligrams"}
            | ("grains","grams") -> {amount = grainsToGrams(x*1m<grain>) |> removeUnit; unitName = "grams"}
            | ("grains","kilograms") -> {amount = grainsToKilograms(x*1m<grain>) |> removeUnit; unitName = "kilograms"}
            | ("drachms","grains") -> {amount = drachmsToGrains(x*1m<drachm>) |> removeUnit; unitName = "grains"}
            | ("drachms","ounces") -> {amount = drachmsToOunces(x*1m<drachm>) |> removeUnit; unitName = "ounces"}
            | ("drachms","pounds") -> {amount = drachmsToPounds(x*1m<drachm>) |> removeUnit; unitName = "pounds"}
            | ("drachms","stones") -> {amount = drachmsToStones(x*1m<drachm>) |> removeUnit; unitName = "stones"}
            | ("drachms","quartersLng") -> {amount = drachmsToQuartersLng(x*1m<drachm>) |> removeUnit; unitName = "quartersLng"}
            | ("drachms","quartersShrt") -> {amount = drachmsToQuartersShrt(x*1m<drachm>) |> removeUnit; unitName = "quartersShrt"}
            | ("drachms","hundredweightsLng") -> {amount = drachmsToHundredweightsLng(x*1m<drachm>) |> removeUnit; unitName = "hundredweightsLng"}
            | ("drachms","hundredweightsShrt") -> {amount = drachmsToHundredweightsShrt(x*1m<drachm>) |> removeUnit; unitName = "hundredweightsShrt"}
            | ("drachms","tonsLng") -> {amount = drachmsToTonsLng(x*1m<drachm>) |> removeUnit; unitName = "tonsLng"}
            | ("drachms","tonsShrt") -> {amount = drachmsToTonsShrt(x*1m<drachm>) |> removeUnit; unitName = "tonsShrt"}
            | ("drachms","tonsMetric") -> {amount = drachmsToTonsMetric(x*1m<drachm>) |> removeUnit; unitName = "tonsMetric"}
            | ("drachms","milligrams") -> {amount = drachmsToMilligrams(x*1m<drachm>) |> removeUnit; unitName = "milligrams"}
            | ("drachms","grams") -> {amount = drachmsToGrams(x*1m<drachm>) |> removeUnit; unitName = "grams"}
            | ("drachms","kilograms") -> {amount = drachmsToKilograms(x*1m<drachm>) |> removeUnit; unitName = "kilograms"}
            | _ -> {amount = 0m; unitName = "conversionNotImplemented"}

        let convertWeight ((x:decimal),fromUnit,toUnit) =
            match List.contains fromUnit UnitsList.weightList && List.contains toUnit UnitsList.weightList with
            | true -> convertWeightVerified(x,fromUnit,toUnit)
            | _ -> {amount = 0m; unitName = "unitNotSupported"}


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