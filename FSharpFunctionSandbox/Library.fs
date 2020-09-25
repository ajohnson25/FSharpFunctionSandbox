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
        let litersPerUkGallon:decimal<liter/gallon_uk> = 4.54609m<liter/gallon_uk>
        let litersPerUsGallon:decimal<liter/gallon_us> = 3.785411784m<liter/gallon_us>
        let usGallonsPerUkGallon:decimal<gallon_us/gallon_uk> = 1.201m<gallon_us/gallon_uk>
        let cubicInchesPerGallonUs: decimal<cubicInch/gallon_us> = 231m<cubicInch/gallon_us>

        //US Volume
        let minimsPerFluidDramUs = 60m<minim_us/fluidDram_us>
        let minimsPerTeaspoonUs = 80m<minim_us/teaspoon_us>
        let teaspoonsPerTablespoonUs = 3m<teaspoon_us/tablespoon_us>
        let tablespoonsPerFluidOunceUs = 2m<tablespoon_us/fluidOunce_us>
        let tablespoonsPerShotUs = 3m<tablespoon_us/shot_us>
        let fluidOuncePerGillUs = 4m<fluidOunce_us/gill_us>
        let gillsPerCupUs = 2m<gill_us/cup_us>
        let cupsPerPintUs = 2m<cup_us/pint_us>
        let pintsPerQuartUs = 2m<pint_us/quart_us>
        let quartsPerPottleUs = 2m<quart_us/pottle_us>
        let pottlesPerGallonUs = 2m<pottle_us/gallon_us>
        let gallonsPerBarrelUs = 31.5m<gallon_us/barrel_us>
        let barrelsPerHogsheadUs = 2m<barrel_us/hogshead_us>

        let minimsUsToFluidDramsUs (min: decimal<minim_us>) = min / minimsPerFluidDramUs
        let fluidDramsUsToMinimsUs (fldr: decimal<fluidDram_us>) = fldr * minimsPerFluidDramUs
        let minimsUsToTeaspoonsUs (min: decimal<minim_us>) = min / minimsPerTeaspoonUs
        let teaspoonsUsToMinimsUs (tsp: decimal<teaspoon_us>) = tsp * minimsPerTeaspoonUs
        let teaspoonsUsToTablespoons (tsp: decimal<teaspoon_us>) = tsp / teaspoonsPerTablespoonUs
        let tablespoonsUsToTeaspoonsUs (tbsp: decimal<tablespoon_us>) = tbsp * teaspoonsPerTablespoonUs
        let fluidOuncesUsToTablespoonsUs (flozUs: decimal<fluidOunce_us>) = flozUs * tablespoonsPerFluidOunceUs
        let tablespoonsUsToFluidOuncesUs (tbsp: decimal<tablespoon_us>) = tbsp / tablespoonsPerFluidOunceUs
        let tablespoonsUsToShotsUs (tbsp: decimal<tablespoon_us>) = tbsp / tablespoonsPerShotUs
        let shotsToTablespoonsUs (shot: decimal<shot_us>) = shot * tablespoonsPerShotUs
        let fluidOuncesUsToGillsUs (flOzUs: decimal<fluidOunce_us>) = flOzUs / fluidOuncePerGillUs
        let gillsUsToFluidOuncesUs (gill_us: decimal<gill_us>) = gill_us * fluidOuncePerGillUs
        let cupsUsToGillsUs (cup: decimal<cup_us>) = cup * gillsPerCupUs
        let gillsUsToCupsUs (gill_us: decimal<gill_us>) = gill_us / gillsPerCupUs
        let cupsUsToPintsUs (cup: decimal<cup_us>) = cup / cupsPerPintUs
        let pintsUsToCupsUs (pint: decimal<pint_us>) = pint * cupsPerPintUs
        let pintsUsToQuartsUs (pint: decimal<pint_us>) = pint / pintsPerQuartUs
        let quartsUsToPintsUs (qt: decimal<quart_us>) = qt * pintsPerQuartUs
        let pottlesUsToQuartsUs (pot: decimal<pottle_us>) = pot * quartsPerPottleUs
        let quartsUsToPottlesUs (qt: decimal<quart_us>) = qt / quartsPerPottleUs
        let pottlesUsToGallonsUs (pot: decimal<pottle_us>) = pot / pottlesPerGallonUs
        let gallonsUsToPottlesUs (gal: decimal<gallon_us>) = gal * pottlesPerGallonUs
        let gallonsUsToBarrelsUs (gal: decimal<gallon_us>) = gal / gallonsPerBarrelUs
        let barrelsUsToGallonsUs (bbl: decimal<barrel_us>) = bbl * gallonsPerBarrelUs
        let barrelsUsToHogsheadsUs (bbl: decimal<barrel_us>) = bbl / barrelsPerHogsheadUs
        let hogsheadsUsToBarrelsUs (hogs: decimal<hogshead_us>) = hogs * barrelsPerHogsheadUs

        let cubicInchesToGallonsUs (cuin: decimal<cubicInch>) = cuin / cubicInchesPerGallonUs
        let gallonsUsToCubicInches (gal: decimal<gallon_us>) = gal * cubicInchesPerGallonUs
        let gallonsUsToLiters (gal: decimal<gallon_us>) = gal * litersPerUsGallon
        let litersToGallonsUs (l: decimal<liter>) = l / litersPerUsGallon

        //UK Volume
        let minimPerFluidDrachmUk = 60m<minim_uk/fluidDrachm_uk>
        let fluidDrachmPerFlOzUk = 8m<fluidDrachm_uk/fluidOunce_uk>
        let fluidOuncePerGillUk = 5m<fluidOunce_uk/gill_uk>
        let gillsPerPintUk = 4m<gill_uk/pint_uk>
        let pintsPerQuartUk = 2m<pint_uk/quart_uk>
        let quartsPerGallonUk = 4m<quart_uk/gallon_uk>
        let gallonsPerPeckUk = 2m<gallon_uk/peck_uk>
        let pecksPerBushelUk = 4m<peck_uk/bushel_uk>
        let bushelsPerQuarterUk = 8m<bushel_uk/quarter_vl_uk>
        let minimsUkToFluidDrachmsUk (min: decimal<minim_uk>) = min / minimPerFluidDrachmUk
        let fluidDrachmsUkToMinimsUk (fldr: decimal<fluidDrachm_uk>) = fldr * minimPerFluidDrachmUk
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
        let pecksUkToGallonsUk (pk: decimal<peck_uk>) = pk * gallonsPerPeckUk
        let gallonsUkToPecksUk (gal: decimal<gallon_uk>) = gal / gallonsPerPeckUk
        let pecksUkToBushelsUk (gal: decimal<peck_uk>) = gal / pecksPerBushelUk
        let bushelsUkToPecksUk (bh: decimal<bushel_uk>) = bh * pecksPerBushelUk
        let bushelsUkToQuartersUk (bh: decimal<bushel_uk>) = bh / bushelsPerQuarterUk
        let quartersUkToBushelsUk (qtr: decimal<quarter_vl_uk>) = qtr * bushelsPerQuarterUk
        let gallonsUkToLiters (gal: decimal<gallon_uk>) = gal * litersPerUkGallon
        let litersToGallonsUk (l: decimal<liter>) = l / litersPerUkGallon

        let gallonsUkToGallonsUs (gal: decimal<gallon_uk>) = gal * usGallonsPerUkGallon
        let gallonsUsToGallonsUk (gal: decimal<gallon_us>) = gal / usGallonsPerUkGallon

        //Metric
        let millilitersPerLiter: decimal<milliliter/liter> = 1000m<milliliter/liter>

        let litersToMilliliters (l: decimal<liter>) = l * millilitersPerLiter
        let millilitersToLiters (ml: decimal<milliliter>) = ml / millilitersPerLiter

        //Cross-conversions
        let usGallonsToUkGallons (usGallon: decimal<gallon_us>) = usGallon / usGallonsPerUkGallon

        let millilitersToGallonsUs x = millilitersToLiters x |> litersToGallonsUs
        let millilitersToPottlesUs x = millilitersToGallonsUs x |> gallonsUsToPottlesUs
        let millilitersToQuartsUs x = millilitersToPottlesUs x |> pottlesUsToQuartsUs
        let millilitersToPintsUs x = millilitersToQuartsUs x |> quartsUsToPintsUs
        let millilitersToCupsUs x = millilitersToPintsUs x |> pintsUsToCupsUs
        let millilitersToGillsUs x = millilitersToCupsUs x |> cupsUsToGillsUs
        let millilitersToFluidOuncesUs x = millilitersToGillsUs x |> gillsUsToFluidOuncesUs
        let millilitersToTablespoonsUs x = millilitersToFluidOuncesUs x |> fluidOuncesUsToTablespoonsUs
        let millilitersToShotsUs x = millilitersToTablespoonsUs x |> tablespoonsUsToShotsUs
        let millilitersToTeaspoonsUs x = millilitersToTablespoonsUs x |> tablespoonsUsToTeaspoonsUs
        let millilitersToMinimsUs x = millilitersToTeaspoonsUs x |> teaspoonsUsToMinimsUs
        let millilitersToFluidDramsUs x = millilitersToMinimsUs x |> minimsUsToFluidDramsUs
        let millilitersToBarrelsUs x = millilitersToGallonsUs x |> gallonsUsToBarrelsUs
        let millilitersToHogsheadsUs x = millilitersToBarrelsUs x |> barrelsUsToHogsheadsUs
        let millilitersToCubicInches x = millilitersToGallonsUs x |> gallonsUsToCubicInches
        let millilitersToGallonsUk x = millilitersToLiters x |> litersToGallonsUk
        let millilitersToPecksUk x = millilitersToGallonsUk x |> gallonsUkToPecksUk
        let millilitersToQuartsUk x = millilitersToGallonsUk x |> gallonsUkToQuartsUk
        let millilitersToPintsUk x = millilitersToQuartsUk x |> quartsUkToPintsUk
        let millilitersToGillsUk x = millilitersToPintsUk x |> pintsUkToGillsUk
        let millilitersToBushelsUk x = millilitersToPecksUk x |> pecksUkToBushelsUk
        let millilitersToQuartersUk x = millilitersToBushelsUk x |> bushelsUkToQuartersUk 
        let millilitersToFluidOuncesUk x = millilitersToGillsUk x |> gillsUkToFluidOuncesUk
        let millilitersToFluidDrachmsUk x = millilitersToFluidOuncesUk x |> fluidOuncesUkToFluidDrachmsUk
        let millilitersToMinimsUk x = millilitersToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUk
        
        let litersToBarrelsUs x = litersToGallonsUs x |> gallonsUsToBarrelsUs
        let litersToCubicInches x = litersToGallonsUs x |> gallonsUsToCubicInches
        let litersToPottlesUs x = litersToGallonsUs x |> gallonsUsToPottlesUs
        let litersToQuartsUs x = litersToPottlesUs x |> pottlesUsToQuartsUs
        let litersToPintsUs x = litersToQuartsUs x |> quartsUsToPintsUs
        let litersToCupsUs x = litersToPintsUs x |> pintsUsToCupsUs
        let litersToGillsUs x = litersToCupsUs x |> cupsUsToGillsUs
        let litersToFluidOuncesUs x = litersToGillsUs x |> gillsUsToFluidOuncesUs
        let litersToTablespoonsUs x = litersToFluidOuncesUs x |> fluidOuncesUsToTablespoonsUs
        let litersToShotsUs x = litersToTablespoonsUs x |> tablespoonsUsToShotsUs
        let litersToTeaspoonsUs x = litersToTablespoonsUs x |> tablespoonsUsToTeaspoonsUs
        let litersToMinimsUs x = litersToTeaspoonsUs x |> teaspoonsUsToMinimsUs
        let litersToFluidDramsUs x = litersToMinimsUs x |> minimsUsToFluidDramsUs
        let litersToHogsheadsUs x = litersToBarrelsUs x |> barrelsUsToHogsheadsUs
        let litersToPecksUk x = litersToGallonsUk x |> gallonsUkToPecksUk
        let litersToBushelsUk x = litersToPecksUk x |> pecksUkToBushelsUk
        let litersToQuartersUk x = litersToBushelsUk x |> bushelsUkToQuartersUk
        let litersToQuartsUk x = litersToGallonsUk x |> gallonsUkToQuartsUk
        let litersToPintsUk x = litersToQuartsUk x |> quartsUkToPintsUk
        let litersToGillsUk x = litersToPintsUk x |> pintsUkToGillsUk
        let litersToFluidOuncesUk x = litersToGillsUk x |> gillsUkToFluidOuncesUk
        let litersToFluidDrachmsUk x = litersToFluidOuncesUk x |> fluidOuncesUkToFluidDrachmsUk
        let litersToMinimsUk x = litersToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUk
        

        let minimsUsToTablespoonsUs x = minimsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let minimsUsToFluidOuncesUs x = minimsUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let minimsUsToShotsUs x = minimsUsToTablespoonsUs x |> tablespoonsUsToShotsUs
        let minimsUsToGillsUs x = minimsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let minimsUsToCupsUs x = minimsUsToGillsUs x |> gillsUsToCupsUs
        let minimsUsToPintsUs x = minimsUsToCupsUs x |> cupsUsToPintsUs
        let minimsUsToQuartsUs x = minimsUsToPintsUs x |> pintsUsToQuartsUs
        let minimsUsToPottlesUs x = minimsUsToQuartsUs x |> quartsUsToPottlesUs
        let minimsUsToGallonsUs x = minimsUsToPottlesUs x |> pottlesUsToGallonsUs
        let minimsUsToCubicInches x = minimsUsToGallonsUs x |> gallonsUsToCubicInches
        let minimsUsToBarrelsUs x = minimsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let minimsUsToLiters x = minimsUsToGallonsUs x |> gallonsUsToLiters
        let minimsUsToMilliliters x = minimsUsToLiters x |> litersToMilliliters
        let minimsUsToGallonsUk x = minimsUsToGallonsUs x |> gallonsUsToGallonsUk
        let minimsUsToPecksUk x = minimsUsToGallonsUk x |> gallonsUkToPecksUk
        let minimsUsToBushelsUk x = minimsUsToPecksUk x |> pecksUkToBushelsUk
        let minimsUsToQuartsUk x = minimsUsToGallonsUk x |> gallonsUkToQuartsUk
        let minimsUsToPintsUk x = minimsUsToQuartsUk x |> quartsUkToPintsUk
        let minimsUsToGillsUk x = minimsUsToPintsUk x |> pintsUkToGillsUk
        let minimsUsToFluidOuncesUk x = minimsUsToGillsUk x |> gillsUkToFluidOuncesUk
        let minimsUsToFluidDrachmsUk x = minimsUsToFluidOuncesUk x |> fluidOuncesUkToFluidDrachmsUk
        let minimsUsToMinimsUk x = minimsUsToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUk
        let minimsUsToHogsheadsUs x = minimsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let minimsUsToQuartersUk x = minimsUsToBushelsUk x |> bushelsUkToQuartersUk

        let minimsUkToFluidOuncesUk x = minimsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let minimsUkToGillsUk x = minimsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let minimsUkToPintsUk x = minimsUkToGillsUk x |> gillsUkToPintsUk
        let minimsUkToQuartsUk x = minimsUkToPintsUk x |> pintsUkToQuartsUk
        let minimsUkToGallonsUk x = minimsUkToQuartsUk x |> quartsUkToGallonsUk
        let minimsUkToPecksUk x = minimsUkToGallonsUk x |> gallonsUkToPecksUk
        let minimsUkToGallonsUs x = minimsUkToGallonsUk x |> gallonsUkToGallonsUs
        let minimsUkToCubicInches x = minimsUkToGallonsUs x |> gallonsUsToCubicInches
        let minimsUkToBarrelsUs x = minimsUkToGallonsUs x |> gallonsUsToBarrelsUs
        let minimsUkToLiters x = minimsUkToGallonsUs x |> gallonsUsToLiters
        let minimsUkToPottlesUs x = minimsUkToGallonsUs x |> gallonsUsToPottlesUs
        let minimsUkToQuartsUs x = minimsUkToPottlesUs x |> pottlesUsToQuartsUs
        let minimsUkToPintsUs x = minimsUkToQuartsUs x |> quartsUsToPintsUs
        let minimsUkToCupsUs x = minimsUkToPintsUs x |> pintsUsToCupsUs
        let minimsUkToGillsUs x = minimsUkToCupsUs x |> cupsUsToGillsUs
        let minimsUkToFluidOuncesUs x = minimsUkToGillsUs x |> gillsUsToFluidOuncesUs
        let minimsUkToTablespoonsUs x = minimsUkToFluidOuncesUs x |> fluidOuncesUsToTablespoonsUs
        let minimsUkToShotsUs x = minimsUkToTablespoonsUs x |> tablespoonsUsToShotsUs
        let minimsUkToTeaspoonsUs x = minimsUkToTablespoonsUs x |> tablespoonsUsToTeaspoonsUs
        let minimsUkToMinimsUs x = minimsUkToTeaspoonsUs x |> teaspoonsUsToMinimsUs
        let minimsUkToFluidDramsUs x = minimsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let minimsUkToHogsheadsUs x = minimsUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let minimsUkToBushelsUk x = minimsUkToPecksUk x |> pecksUkToBushelsUk
        let minimsUkToQuartersUk x = minimsUkToBushelsUk x |> bushelsUkToQuartersUk
        let minimsUkToMilliliters x = minimsUkToLiters x |> litersToMilliliters

        let fluidDramsUsToTeaspoonsUs x = fluidDramsUsToMinimsUs x |> minimsUsToTeaspoonsUs
        let fluidDramsUsToTablespoonsUs x = fluidDramsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let fluidDramsUsToFluidOuncesUs x = fluidDramsUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let fluidDramsUsToShotsUs x = fluidDramsUsToTablespoonsUs x |> tablespoonsUsToShotsUs
        let fluidDramsUsToGillsUs x = fluidDramsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let fluidDramsUsToCupsUs x = fluidDramsUsToGillsUs x |> gillsUsToCupsUs
        let fluidDramsUsToPintsUs x = fluidDramsUsToCupsUs x |> cupsUsToPintsUs
        let fluidDramsUsToQuartsUs x = fluidDramsUsToPintsUs x |> pintsUsToQuartsUs
        let fluidDramsUsToPottlesUs x = fluidDramsUsToQuartsUs x |> quartsUsToPottlesUs
        let fluidDramsUsToGallonsUs x = fluidDramsUsToPottlesUs x |> pottlesUsToGallonsUs
        let fluidDramsUsToBarrelsUs x = fluidDramsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let fluidDramsUsToHogsheadsUs x = fluidDramsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let fluidDramsUsToCubicInches x = fluidDramsUsToGallonsUs x |> gallonsUsToCubicInches
        let fluidDramsUsToMinimsUk x = fluidDramsUsToMinimsUs x |> minimsUsToMinimsUk
        let fluidDramsUsToFluidDrachmsUk x = fluidDramsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let fluidDramsUsToFluidOuncesUk x = fluidDramsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let fluidDramsUsToGillsUk x = fluidDramsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let fluidDramsUsToPintsUk x = fluidDramsUsToGillsUk x |> gillsUkToPintsUk
        let fluidDramsUsToQuartsUk x = fluidDramsUsToPintsUk x |> pintsUkToQuartsUk
        let fluidDramsUsToGallonsUk x = fluidDramsUsToQuartsUk x |> quartsUkToGallonsUk
        let fluidDramsUsToPecksUk x = fluidDramsUsToGallonsUk x |> gallonsUkToPecksUk
        let fluidDramsUsToBushelsUk x = fluidDramsUsToPecksUk x |> pecksUkToBushelsUk
        let fluidDramsUsToQuartersUk x = fluidDramsUsToBushelsUk x |> bushelsUkToQuartersUk
        let fluidDramsUsToMilliliters x = fluidDramsUsToMinimsUs x |> minimsUsToMilliliters
        let fluidDramsUsToLiters x = fluidDramsUsToMilliliters x |> millilitersToLiters

        let fluidDrachmsUkToMinimsUs x = fluidDrachmsUkToMinimsUk x |> minimsUkToMinimsUs
        let fluidDrachmsUkToFluidDramsUs x = fluidDrachmsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let fluidDrachmsUkToTeaspoonsUs x = fluidDrachmsUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let fluidDrachmsUkToTablespoonsUs x = fluidDrachmsUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let fluidDrachmsUkToFluidOuncesUs x = fluidDrachmsUkToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let fluidDrachmsUkToShotsUs x = fluidDrachmsUkToTablespoonsUs x |> tablespoonsUsToShotsUs
        let fluidDrachmsUkToGillsUs x = fluidDrachmsUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let fluidDrachmsUkToCupsUs x = fluidDrachmsUkToGillsUs x |> gillsUsToCupsUs
        let fluidDrachmsUkToPintsUs x = fluidDrachmsUkToCupsUs x |> cupsUsToPintsUs
        let fluidDrachmsUkToQuartsUs x = fluidDrachmsUkToPintsUs x |> pintsUsToQuartsUs
        let fluidDrachmsUkToPottlesUs x = fluidDrachmsUkToQuartsUs x |> quartsUsToPottlesUs
        let fluidDrachmsUkToGallonsUs x = fluidDrachmsUkToPottlesUs x |> pottlesUsToGallonsUs
        let fluidDrachmsUkToBarrelsUs x = fluidDrachmsUkToGallonsUs x |> gallonsUsToBarrelsUs
        let fluidDrachmsUkToHogsheadsUs x = fluidDrachmsUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let fluidDrachmsUkToCubicInches x = fluidDrachmsUkToGallonsUs x |> gallonsUsToCubicInches
        let fluidDrachmsUkToGillsUk x = fluidDrachmsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let fluidDrachmsUkToPintsUk x = fluidDrachmsUkToGillsUk x |> gillsUkToPintsUk
        let fluidDrachmsUkToQuartsUk x = fluidDrachmsUkToPintsUk x |> pintsUkToQuartsUk
        let fluidDrachmsUkToGallonsUk x = fluidDrachmsUkToQuartsUk x |> quartsUkToGallonsUk
        let fluidDrachmsUkToPecksUk x = fluidDrachmsUkToGallonsUk x |> gallonsUkToPecksUk
        let fluidDrachmsUkToBushelsUk x = fluidDrachmsUkToPecksUk x |> pecksUkToBushelsUk
        let fluidDrachmsUkToQuartersUk x = fluidDrachmsUkToBushelsUk x |> bushelsUkToQuartersUk
        let fluidDrachmsUkToMilliliters x = fluidDrachmsUkToMinimsUk x |> minimsUkToMilliliters
        let fluidDrachmsUkToLiters x = fluidDrachmsUkToMilliliters x |> millilitersToLiters

        let teaspoonsUsToFluidDramsUs x = teaspoonsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let teaspoonsUsToFluidOuncesUs x = teaspoonsUsToTablespoons x |> tablespoonsUsToFluidOuncesUs
        let teaspoonsUsToShotsUs x = teaspoonsUsToTablespoons x |> tablespoonsUsToShotsUs
        let teaspoonsUsToGillsUs x = teaspoonsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let teaspoonsUsToCupsUs x = teaspoonsUsToGillsUs x |> gillsUsToCupsUs
        let teaspoonsUsToPintsUs x = teaspoonsUsToCupsUs x |> cupsUsToPintsUs
        let teaspoonsUsToQuartsUs x = teaspoonsUsToPintsUs x |> pintsUsToQuartsUs
        let teaspoonsUsToPottlesUs x = teaspoonsUsToQuartsUs x |> quartsUsToPottlesUs
        let teaspoonsUsToGallonsUs x = teaspoonsUsToPottlesUs x |> pottlesUsToGallonsUs
        let teaspoonsUsToBarrelsUs x = teaspoonsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let teaspoonsUsToHogsheadsUs x = teaspoonsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let teaspoonsUsToCubicInches x = teaspoonsUsToGallonsUs x |> gallonsUsToCubicInches
        let teaspoonsUsToMinimsUk x = teaspoonsUsToMinimsUs x |> minimsUsToMinimsUk
        let teaspoonsUsToFluidDrachmsUk x = teaspoonsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let teaspoonsUsToFluidOuncesUk x = teaspoonsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let teaspoonsUsToGillsUk x = teaspoonsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let teaspoonsUsToPintsUk x = teaspoonsUsToGillsUk x |> gillsUkToPintsUk
        let teaspoonsUsToQuartsUk x = teaspoonsUsToPintsUk x |> pintsUkToQuartsUk
        let teaspoonsUsToGallonsUk x = teaspoonsUsToQuartsUk x |> quartsUkToGallonsUk
        let teaspoonsUsToPecksUk x = teaspoonsUsToGallonsUk x |> gallonsUkToPecksUk
        let teaspoonsUsToBushelsUk x = teaspoonsUsToPecksUk x |> pecksUkToBushelsUk
        let teaspoonsUsToQuartersUk x = teaspoonsUsToBushelsUk x |> bushelsUkToQuartersUk
        let teaspoonsUsToMilliliters x = teaspoonsUsToMinimsUs x |> minimsUsToMilliliters
        let teaspoonsUsToLiters x = teaspoonsUsToMilliliters x |> millilitersToLiters

        let tablespoonsUsToMinimsUs x = tablespoonsUsToTeaspoonsUs x |> teaspoonsUsToMinimsUs
        let tablespoonsUsToFluidDramsUs x = tablespoonsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let tablespoonsUsToGillsUs x = tablespoonsUsToFluidDramsUs x |> fluidDramsUsToGillsUs
        let tablespoonsUsToCupsUs x = tablespoonsUsToGillsUs x |> gillsUsToCupsUs
        let tablespoonsUsToPintsUs x = tablespoonsUsToCupsUs x |> cupsUsToPintsUs
        let tablespoonsUsToQuartsUs x = tablespoonsUsToPintsUs x |> pintsUsToQuartsUs
        let tablespoonsUsToPottlesUs x = tablespoonsUsToQuartsUs x |> quartsUsToPottlesUs
        let tablespoonsUsToGallonsUs x = tablespoonsUsToPottlesUs x |> pottlesUsToGallonsUs
        let tablespoonsUsToBarrelsUs x = tablespoonsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let tablespoonsUsToHogsheadsUs x = tablespoonsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let tablespoonsUsToCubicInches x = tablespoonsUsToGallonsUs x |> gallonsUsToCubicInches
        let tablespoonsUsToMinimsUk x = tablespoonsUsToMinimsUs x |> minimsUsToMinimsUk
        let tablespoonsUsToFluidDrachmsUk x = tablespoonsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let tablespoonsUsToFluidOuncesUk x = tablespoonsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let tablespoonsUsToGillsUk x = tablespoonsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let tablespoonsUsToPintsUk x = tablespoonsUsToGillsUk x |> gillsUkToPintsUk
        let tablespoonsUsToQuartsUk x = tablespoonsUsToPintsUk x |> pintsUkToQuartsUk
        let tablespoonsUsToGallonsUk x = tablespoonsUsToQuartsUk x |> quartsUkToGallonsUk
        let tablespoonsUsToPecksUk x = tablespoonsUsToGallonsUk x |> gallonsUkToPecksUk
        let tablespoonsUsToBushelsUk x = tablespoonsUsToPecksUk x |> pecksUkToBushelsUk
        let tablespoonsUsToQuartersUk x = tablespoonsUsToBushelsUk x |> bushelsUkToQuartersUk
        let tablespoonsUsToMilliliters x = tablespoonsUsToMinimsUs x |> minimsUsToMilliliters
        let tablespoonsUsToLiters x = tablespoonsUsToMilliliters x |> millilitersToLiters

        let fluidOuncesUsToMinimsUs x = fluidOuncesUsToTablespoonsUs x |> tablespoonsUsToMinimsUs
        let fluidOuncesUsToFluidDramsUs x = fluidOuncesUsToMinimsUs x |> minimsUsToFluidDramsUs
        let fluidOuncesUsToTeaspoonsUs x = fluidOuncesUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let fluidOuncesUsToShotsUs x = fluidOuncesUsToTeaspoonsUs x |> teaspoonsUsToShotsUs
        let fluidOuncesUsToCupsUs x = fluidOuncesUsToTablespoonsUs x |> tablespoonsUsToCupsUs
        let fluidOuncesUsToPintsUs x = fluidOuncesUsToCupsUs x |> cupsUsToPintsUs
        let fluidOuncesUsToQuartsUs x = fluidOuncesUsToPintsUs x |> pintsUsToQuartsUs
        let fluidOuncesUsToPottlesUs x = fluidOuncesUsToQuartsUs x |> quartsUsToPottlesUs
        let fluidOuncesUsToGallonsUs x = fluidOuncesUsToPottlesUs x |> pottlesUsToGallonsUs
        let fluidOuncesUsToBarrelsUs x = fluidOuncesUsToGallonsUs x |> gallonsUsToBarrelsUs
        let fluidOuncesUsToHogsheadsUs x = fluidOuncesUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let fluidOuncesUsToCubicInches x = fluidOuncesUsToGallonsUs x |> gallonsUsToCubicInches
        let fluidOuncesUsToMinimsUk x = fluidOuncesUsToMinimsUs x |> minimsUsToMinimsUk
        let fluidOuncesUsToFluidDrachmsUk x = fluidOuncesUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let fluidOuncesUsToFluidOuncesUk x = fluidOuncesUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let fluidOuncesUsToGillsUk x = fluidOuncesUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let fluidOuncesUsToPintsUk x = fluidOuncesUsToGillsUk x |> gillsUkToPintsUk
        let fluidOuncesUsToQuartsUk x = fluidOuncesUsToPintsUk x |> pintsUkToQuartsUk
        let fluidOuncesUsToGallonsUk x = fluidOuncesUsToQuartsUk x |> quartsUkToGallonsUk
        let fluidOuncesUsToPecksUk x = fluidOuncesUsToGallonsUk x |> gallonsUkToPecksUk
        let fluidOuncesUsToBushelsUk x = fluidOuncesUsToPecksUk x |> pecksUkToBushelsUk
        let fluidOuncesUsToQuartersUk x = fluidOuncesUsToBushelsUk x |> bushelsUkToQuartersUk
        let fluidOuncesUsToMilliliters x = fluidOuncesUsToMinimsUs x |> minimsUsToMilliliters
        let fluidOuncesUsToLiters x = fluidOuncesUsToMilliliters x |> millilitersToLiters

        let fluidOuncesUkToMinimsUs x = fluidOuncesUkToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUs
        let fluidOuncesUkToFluidOuncesUs x = fluidOuncesUkToMinimsUs x |> minimsUsToFluidOuncesUs
        let fluidOuncesUkToFluidDramsUs x = fluidOuncesUkToMinimsUs x |> minimsUsToFluidDramsUs
        let fluidOuncesUkToTeaspoonsUs x = fluidOuncesUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let fluidOuncesUkToTablespoonsUs x = fluidOuncesUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let fluidOuncesUkToShotsUs x = fluidOuncesUkToTablespoonsUs x |> tablespoonsUsToShotsUs
        let fluidOuncesUkToGillsUs x = fluidOuncesUkToTablespoonsUs x |> tablespoonsUsToGillsUs
        let fluidOuncesUkToCupsUs x = fluidOuncesUkToGillsUs x |> gillsUsToCupsUs
        let fluidOuncesUkToPintsUs x = fluidOuncesUkToCupsUs x |> cupsUsToPintsUs
        let fluidOuncesUkToQuartsUs x = fluidOuncesUkToPintsUs x |> pintsUsToQuartsUs
        let fluidOuncesUkToPottlesUs x = fluidOuncesUkToQuartsUs x |> quartsUsToPottlesUs
        let fluidOuncesUkToGallonsUs x = fluidOuncesUkToPottlesUs x |> pottlesUsToGallonsUs
        let fluidOuncesUkToBarrelsUs x = fluidOuncesUkToGallonsUs x |> gallonsUsToBarrelsUs
        let fluidOuncesUkToHogsheadsUs x = fluidOuncesUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let fluidOuncesUkToCubicInches x = fluidOuncesUkToGallonsUs x |> gallonsUsToCubicInches
        let fluidOuncesUkToMinimsUk x = fluidOuncesUkToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUk
        let fluidOuncesUkToPintsUk x = fluidOuncesUkToGillsUk x |> gillsUkToPintsUk
        let fluidOuncesUkToQuartsUk x = fluidOuncesUkToPintsUk x |> pintsUkToQuartsUk
        let fluidOuncesUkToGallonsUk x = fluidOuncesUkToQuartsUk x |> quartsUkToGallonsUk
        let fluidOuncesUkToPecksUk x = fluidOuncesUkToGallonsUk x |> gallonsUkToPecksUk
        let fluidOuncesUkToBushelsUk x = fluidOuncesUkToPecksUk x |> pecksUkToBushelsUk
        let fluidOuncesUkToQuartersUk x = fluidOuncesUkToBushelsUk x |> bushelsUkToQuartersUk
        let fluidOuncesUkToMilliliters x = fluidOuncesUkToMinimsUk x |> minimsUkToMilliliters
        let fluidOuncesUkToLiters x = fluidOuncesUkToMilliliters x |> millilitersToLiters

        let gillsUsToMinimsUs x = gillsUsToFluidOuncesUs x |> fluidOuncesUsToMinimsUs
        let gillsUsToFluidDramsUs x = gillsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let gillsUsToTeaspoonsUs x = gillsUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let gillsUsToTablespoonsUs x = gillsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let gillsUsToShotsUs x = gillsUsToTablespoonsUs x |> tablespoonsUsToShotsUs
        let gillsUsToPintsUs x = gillsUsToCupsUs x |> cupsUsToPintsUs
        let gillsUsToQuartsUs x = gillsUsToPintsUs x |> pintsUsToQuartsUs
        let gillsUsToPottlesUs x = gillsUsToQuartsUs x |> quartsUsToPottlesUs
        let gillsUsToGallonsUs x = gillsUsToPottlesUs x |> pottlesUsToGallonsUs
        let gillsUsToBarrelsUs x = gillsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let gillsUsToHogsheadsUs x = gillsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let gillsUsToCubicInches x = gillsUsToGallonsUs x |> gallonsUsToCubicInches
        let gillsUsToMinimsUk x = gillsUsToMinimsUs x |> minimsUsToMinimsUk
        let gillsUsToFluidDrachmsUk x = gillsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let gillsUsToFluidOuncesUk x = gillsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let gillsUsToGillsUk x = gillsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let gillsUsToPintsUk x = gillsUsToGillsUk x |> gillsUkToPintsUk
        let gillsUsToQuartsUk x = gillsUsToPintsUk x |> pintsUkToQuartsUk
        let gillsUsToGallonsUk x = gillsUsToQuartsUk x |> quartsUkToGallonsUk
        let gillsUsToPecksUk x = gillsUsToGallonsUk x |> gallonsUkToPecksUk
        let gillsUsToBushelsUk x = gillsUsToPecksUk x |> pecksUkToBushelsUk
        let gillsUsToQuartersUk x = gillsUsToBushelsUk x |> bushelsUkToQuartersUk
        let gillsUsToMilliliters x = gillsUsToMinimsUs x |> minimsUsToMilliliters
        let gillsUsToLiters x = gillsUsToMilliliters x |> millilitersToLiters

        let gillsUkToMinimsUs x = gillsUkToFluidOuncesUk x |> fluidOuncesUkToMinimsUs
        let gillsUkToFluidDramsUs x = gillsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let gillsUkToTeaspoonsUs x = gillsUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let gillsUkToTablespoonsUs x = gillsUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let gillsUkToFluidOuncesUs x = gillsUkToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let gillsUkToShotsUs x = gillsUkToTablespoonsUs x |> tablespoonsUsToShotsUs
        let gillsUkToGillsUs x = gillsUkToTeaspoonsUs x |> teaspoonsUsToGillsUs
        let gillsUkToCupsUs x = gillsUkToGillsUs x |> gillsUsToCupsUs
        let gillsUkToPintsUs x = gillsUkToCupsUs x |> cupsUsToPintsUs
        let gillsUkToQuartsUs x = gillsUkToPintsUs x |> pintsUsToQuartsUs
        let gillsUkToPottlesUs x = gillsUkToQuartsUs x |> quartsUsToPottlesUs
        let gillsUkToGallonsUs x = gillsUkToPottlesUs x |> pottlesUsToGallonsUs
        let gillsUkToBarrelsUs x = gillsUkToGallonsUs x |> gallonsUsToBarrelsUs
        let gillsUkToHogsheadsUs x = gillsUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let gillsUkToCubicInches x = gillsUkToGallonsUs x |> gallonsUsToCubicInches
        let gillsUkToMinimsUk x = gillsUkToFluidOuncesUk x |> fluidOuncesUkToMinimsUk
        let gillsUkToFluidDrachmsUk x = gillsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let gillsUkToQuartsUk x = gillsUkToPintsUk x |> pintsUkToQuartsUk
        let gillsUkToGallonsUk x = gillsUkToQuartsUk x |> quartsUkToGallonsUk
        let gillsUkToPecksUk x = gillsUkToGallonsUk x |> gallonsUkToPecksUk
        let gillsUkToBushelsUk x = gillsUkToPecksUk x |> pecksUkToBushelsUk
        let gillsUkToQuartersUk x = gillsUkToBushelsUk x |> bushelsUkToQuartersUk
        let gillsUkToMilliliters x = gillsUkToMinimsUk x |> minimsUkToMilliliters
        let gillsUkToLiters x = gillsUkToMilliliters x |> millilitersToLiters

        let shotsUsToMinimsUs x = shotsToTablespoonsUs x |> tablespoonsUsToMinimsUs
        let shotsUsToFluidDramsUs x = shotsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let shotsUsToTeaspoonsUs x = shotsUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let shotsUsToFluidOuncesUs x = shotsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let shotsUsToGillsUs x = shotsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let shotsUsToCupsUs x = shotsUsToGillsUs x |> gillsUsToCupsUs
        let shotsUsToPintsUs x = shotsUsToCupsUs x |> cupsUsToPintsUs
        let shotsUsToQuartsUs x = shotsUsToPintsUs x |> pintsUsToQuartsUs
        let shotsUsToPottlesUs x = shotsUsToQuartsUs x |> quartsUsToPottlesUs
        let shotsUsToGallonsUs x = shotsUsToPottlesUs x |> pottlesUsToGallonsUs
        let shotsUsToBarrelsUs x = shotsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let shotsUsToHogsheadsUs x = shotsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let shotsUsToCubicInches x = shotsUsToGallonsUs x |> gallonsUsToCubicInches
        let shotsUsToMinimsUk x = shotsUsToMinimsUs x |> minimsUsToMinimsUk
        let shotsUsToFluidDrachmsUk x = shotsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let shotsUsToFluidOuncesUk x = shotsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let shotsUsToGillsUk x = shotsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let shotsUsToPintsUk x = shotsUsToGillsUk x |> gillsUkToPintsUk
        let shotsUsToQuartsUk x = shotsUsToPintsUk x |> pintsUkToQuartsUk
        let shotsUsToGallonsUk x = shotsUsToQuartsUk x |> quartsUkToGallonsUk
        let shotsUsToPecksUk x = shotsUsToGallonsUk x |> gallonsUkToPecksUk
        let shotsUsToBushelsUk x = shotsUsToPecksUk x |> pecksUkToBushelsUk
        let shotsUsToQuartersUk x = shotsUsToBushelsUk x |> bushelsUkToQuartersUk
        let shotsUsToMilliliters x = shotsUsToMinimsUs x |> minimsUsToMilliliters
        let shotsUsToLiters x = shotsUsToMilliliters x |> millilitersToLiters

        let cupsUsToMinimsUs x = cupsUsToGillsUs x |> gillsUsToMinimsUs
        let cupsUsToFluidDramsUs x = cupsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let cupsUsToTeaspoonsUs x = cupsUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let cupsUsToTablespoonsUs x = cupsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let cupsUsToFluidOuncesUs x = cupsUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let cupsUsToShotsUs x = cupsUsToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let cupsUsToQuartsUs x = cupsUsToPintsUs x |> pintsUsToQuartsUs
        let cupsUsToPottlesUs x = cupsUsToQuartsUs x |> quartsUsToPottlesUs
        let cupsUsToGallonsUs x = cupsUsToPottlesUs x |> pottlesUsToGallonsUs
        let cupsUsToBarrelsUs x = cupsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let cupsUsToHogsheadsUs x = cupsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let cupsUsToCubicInches x = cupsUsToGallonsUs x |> gallonsUsToCubicInches
        let cupsUsToMinimsUk x = cupsUsToMinimsUs x |> minimsUsToMinimsUk
        let cupsUsToFluidDrachmsUk x = cupsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let cupsUsToFluidOuncesUk x = cupsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let cupsUsToGillsUk x = cupsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let cupsUsToPintsUk x = cupsUsToGillsUk x |> gillsUkToPintsUk
        let cupsUsToQuartsUk x = cupsUsToPintsUk x |> pintsUkToQuartsUk
        let cupsUsToGallonsUk x = cupsUsToQuartsUk x |> quartsUkToGallonsUk
        let cupsUsToPecksUk x = cupsUsToGallonsUk x |> gallonsUkToPecksUk
        let cupsUsToBushelsUk x = cupsUsToPecksUk x |> pecksUkToBushelsUk
        let cupsUsToQuartersUk x = cupsUsToBushelsUk x |> bushelsUkToQuartersUk
        let cupsUsToMilliliters x = cupsUsToMinimsUs x |> minimsUsToMilliliters
        let cupsUsToLiters x = cupsUsToMilliliters x |> millilitersToLiters

        let pintsUsToMinimsUs x = pintsUsToCupsUs x |> cupsUsToMinimsUs
        let pintsUsToFluidDramsUs x = pintsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let pintsUsToTeaspoonsUs x = pintsUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let pintsUsToTablespoonsUs x = pintsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let pintsUsToFluidOuncesUs x = pintsUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let pintsUsToShotsUs x = pintsUsToTablespoonsUs x |> tablespoonsUsToShotsUs
        let pintsUsToGillsUs x = pintsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let pintsUsToPottlesUs x = pintsUsToQuartsUs x |> quartsUsToPottlesUs
        let pintsUsToGallonsUs x = pintsUsToPottlesUs x |> pottlesUsToGallonsUs
        let pintsUsToBarrelsUs x = pintsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let pintsUsToHogsheadsUs x = pintsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let pintsUsToCubicInches x = pintsUsToGallonsUs x |> gallonsUsToCubicInches
        let pintsUsToMinimsUk x = pintsUsToMinimsUs x |> minimsUsToMinimsUk
        let pintsUsToFluidDrachmsUk x = pintsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let pintsUsToFluidOuncesUk x = pintsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let pintsUsToGillsUk x = pintsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let pintsUsToPintsUk x = pintsUsToGillsUk x |> gillsUkToPintsUk
        let pintsUsToQuartsUk x = pintsUsToPintsUk x |> pintsUkToQuartsUk
        let pintsUsToGallonsUk x = pintsUsToQuartsUk x |> quartsUkToGallonsUk
        let pintsUsToPecksUk x = pintsUsToGallonsUk x |> gallonsUkToPecksUk
        let pintsUsToBushelsUk x = pintsUsToPecksUk x |> pecksUkToBushelsUk
        let pintsUsToQuartersUk x = pintsUsToBushelsUk x |> bushelsUkToQuartersUk
        let pintsUsToMilliliters x = pintsUsToMinimsUs x |> minimsUsToMilliliters
        let pintsUsToLiters x = pintsUsToMilliliters x |> millilitersToLiters

        let pintsUkToMinimsUs x = pintsUkToGillsUk x |> gillsUkToMinimsUs
        let pintsUkToFluidDramsUs x = pintsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let pintsUkToTeaspoonsUs x = pintsUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let pintsUkToTablespoonsUs x = pintsUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let pintsUkToFluidOuncesUs x = pintsUkToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let pintsUkToShotsUs x = pintsUkToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let pintsUkToGillsUs x = pintsUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let pintsUkToCupsUs x = pintsUkToGillsUs x |> gillsUsToCupsUs
        let pintsUkToPintsUs x = pintsUkToCupsUs x |> cupsUsToPintsUs
        let pintsUkToQuartsUs x = pintsUkToPintsUs x |> pintsUsToQuartsUs
        let pintsUkToPottlesUs x = pintsUkToQuartsUs x |> quartsUsToPottlesUs
        let pintsUkToGallonsUs x = pintsUkToPottlesUs x |> pottlesUsToGallonsUs
        let pintsUkToBarrelsUs x = pintsUkToGallonsUs x |> gallonsUsToBarrelsUs
        let pintsUkToHogsheadsUs x = pintsUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let pintsUkToCubicInches x = pintsUkToGallonsUs x |> gallonsUsToCubicInches
        let pintsUkToMinimsUk x = pintsUkToGillsUk x |> gillsUkToMinimsUk
        let pintsUkToFluidDrachmsUk x = pintsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let pintsUkToFluidOuncesUk x = pintsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let pintsUkToGallonsUk x = pintsUkToQuartsUk x |> quartsUkToGallonsUk
        let pintsUkToPecksUk x = pintsUkToGallonsUk x |> gallonsUkToPecksUk
        let pintsUkToBushelsUk x = pintsUkToPecksUk x |> pecksUkToBushelsUk
        let pintsUkToQuartersUk x = pintsUkToBushelsUk x |> bushelsUkToQuartersUk
        let pintsUkToMilliliters x = pintsUkToMinimsUk x |> minimsUkToMilliliters
        let pintsUkToLiters x = pintsUkToMilliliters x |> millilitersToLiters

        let quartsUsToMinimsUs x = quartsUsToPintsUs x |> pintsUsToMinimsUs
        let quartsUsToFluidDramsUs x = quartsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let quartsUsToTeaspoonsUs x = quartsUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let quartsUsToTablespoonsUs x = quartsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let quartsUsToFluidOuncesUs x = quartsUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let quartsUsToShotsUs x = quartsUsToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let quartsUsToGillsUs x = quartsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let quartsUsToCupsUs x = quartsUsToGillsUs x |> gillsUsToCupsUs
        let quartsUsToGallonsUs x = quartsUsToPottlesUs x |> pottlesUsToGallonsUs
        let quartsUsToBarrelsUs x = quartsUsToGallonsUs x |> gallonsUsToBarrelsUs
        let quartsUsToHogsheadsUs x = quartsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let quartsUsToCubicInches x = quartsUsToGallonsUs x |> gallonsUsToCubicInches
        let quartsUsToMinimsUk x = quartsUsToMinimsUs x |> minimsUsToMinimsUk
        let quartsUsToFluidDrachmsUk x = quartsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let quartsUsToFluidOuncesUk x = quartsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let quartsUsToGillsUk x = quartsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let quartsUsToPintsUk x = quartsUsToGillsUk x |> gillsUkToPintsUk
        let quartsUsToQuartsUk x = quartsUsToPintsUk x |> pintsUkToQuartsUk
        let quartsUsToGallonsUk x = quartsUsToQuartsUk x |> quartsUkToGallonsUk
        let quartsUsToPecksUk x = quartsUsToGallonsUk x |> gallonsUkToPecksUk
        let quartsUsToBushelsUk x = quartsUsToPecksUk x |> pecksUkToBushelsUk
        let quartsUsToQuartersUk x = quartsUsToBushelsUk x |> bushelsUkToQuartersUk
        let quartsUsToMilliliters x = quartsUsToMinimsUs x |> minimsUsToMilliliters
        let quartsUsToLiters x = quartsUsToMilliliters x |> millilitersToLiters

        let quartsUkToMinimsUs x = quartsUkToPintsUk x |> pintsUkToMinimsUs
        let quartsUkToFluidDramsUs x = quartsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let quartsUkToTeaspoonsUs x = quartsUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let quartsUkToTablespoonsUs x = quartsUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let quartsUkToFluidOuncesUs x = quartsUkToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let quartsUkToShotsUs x = quartsUkToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let quartsUkToGillsUs x = quartsUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let quartsUkToCupsUs x = quartsUkToGillsUs x |> gillsUsToCupsUs
        let quartsUkToPintsUs x = quartsUkToCupsUs x |> cupsUsToPintsUs
        let quartsUkToQuartsUs x = quartsUkToPintsUs x |> pintsUsToQuartsUs
        let quartsUkToPottlesUs x = quartsUkToQuartsUs x |> quartsUsToPottlesUs
        let quartsUkToGallonsUs x = quartsUkToPottlesUs x |> pottlesUsToGallonsUs
        let quartsUkToBarrelsUs x = quartsUkToGallonsUs x |> gallonsUsToBarrelsUs
        let quartsUkToHogsheadsUs x = quartsUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let quartsUkToCubicInches x = quartsUkToGallonsUs x |> gallonsUsToCubicInches
        let quartsUkToMinimsUk x = quartsUkToPintsUk x |> pintsUkToMinimsUk
        let quartsUkToFluidDrachmsUk x = quartsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let quartsUkToFluidOuncesUk x = quartsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let quartsUkToGillsUk x = quartsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let quartsUkToPecksUk x = quartsUkToGallonsUk x |> gallonsUkToPecksUk
        let quartsUkToBushelsUk x = quartsUkToPecksUk x |> pecksUkToBushelsUk
        let quartsUkToQuartersUk x = quartsUkToBushelsUk x |> bushelsUkToQuartersUk
        let quartsUkToMilliliters x = quartsUkToMinimsUk x |> minimsUkToMilliliters
        let quartsUkToLiters x = quartsUkToMilliliters x |> millilitersToLiters

        let pottlesUsToMinimsUs x = pottlesUsToQuartsUs x |> quartsUsToMinimsUs
        let pottlesUsToFluidDramsUs x = pottlesUsToMinimsUs x |> minimsUsToFluidDramsUs
        let pottlesUsToTeaspoonsUs x = pottlesUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let pottlesUsToTablespoonsUs x = pottlesUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let pottlesUsToFluidOuncesUs x = pottlesUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let pottlesUsToShotsUs x = pottlesUsToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let pottlesUsToGillsUs x = pottlesUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let pottlesUsToCupsUs x = pottlesUsToGillsUs x |> gillsUsToCupsUs
        let pottlesUsToPintsUs x = pottlesUsToCupsUs x |> cupsUsToPintsUs
        let pottlesUsToBarrelsUs x = pottlesUsToGallonsUs x |> gallonsUsToBarrelsUs
        let pottlesUsToHogsheadsUs x = pottlesUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let pottlesUsToCubicInches x = pottlesUsToGallonsUs x |> gallonsUsToCubicInches
        let pottlesUsToMinimsUk x = pottlesUsToMinimsUs x |> minimsUsToMinimsUk
        let pottlesUsToFluidDrachmsUk x = pottlesUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let pottlesUsToFluidOuncesUk x = pottlesUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let pottlesUsToGillsUk x = pottlesUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let pottlesUsToPintsUk x = pottlesUsToGillsUk x |> gillsUkToPintsUk
        let pottlesUsToQuartsUk x = pottlesUsToPintsUk x |> pintsUkToQuartsUk
        let pottlesUsToGallonsUk x = pottlesUsToQuartsUk x |> quartsUkToGallonsUk
        let pottlesUsToPecksUk x = pottlesUsToGallonsUk x |> gallonsUkToPecksUk
        let pottlesUsToBushelsUk x = pottlesUsToPecksUk x |> pecksUkToBushelsUk
        let pottlesUsToQuartersUk x = pottlesUsToBushelsUk x |> bushelsUkToQuartersUk
        let pottlesUsToMilliliters x = pottlesUsToMinimsUs x |> minimsUsToMilliliters
        let pottlesUsToLiters x = pottlesUsToMilliliters x |> millilitersToLiters

        let gallonsUsToMinimsUs x = gallonsUsToPottlesUs x |> pottlesUsToMinimsUs
        let gallonsUsToFluidDramsUs x = gallonsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let gallonsUsToTeaspoonsUs x = gallonsUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let gallonsUsToTablespoonsUs x = gallonsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let gallonsUsToFluidOuncesUs x = gallonsUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let gallonsUsToShotsUs x = gallonsUsToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let gallonsUsToGillsUs x = gallonsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let gallonsUsToCupsUs x = gallonsUsToGillsUs x |> gillsUsToCupsUs
        let gallonsUsToPintsUs x = gallonsUsToCupsUs x |> cupsUsToPintsUs
        let gallonsUsToQuartsUs x = gallonsUsToPintsUs x |> pintsUsToQuartsUs
        let gallonsUsToHogsheadsUs x = gallonsUsToBarrelsUs x |> barrelsUsToHogsheadsUs
        let gallonsUsToMinimsUk x = gallonsUsToMinimsUs x |> minimsUsToMinimsUk
        let gallonsUsToFluidDrachmsUk x = gallonsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let gallonsUsToFluidOuncesUk x = gallonsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let gallonsUsToGillsUk x = gallonsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let gallonsUsToPintsUk x = gallonsUsToGillsUk x |> gillsUkToPintsUk
        let gallonsUsToQuartsUk x = gallonsUsToPintsUk x |> pintsUkToQuartsUk
        let gallonsUsToPecksUk x = gallonsUsToGallonsUk x |> gallonsUkToPecksUk
        let gallonsUsToBushelsUk x = gallonsUsToPecksUk x |> pecksUkToBushelsUk
        let gallonsUsToQuartersUk x = gallonsUsToBushelsUk x |> bushelsUkToQuartersUk
        let gallonsUsToMilliliters x = gallonsUsToLiters x |> litersToMilliliters

        let gallonsUkToMinimsUs x = gallonsUkToGallonsUs x |> gallonsUsToMinimsUs
        let gallonsUkToFluidDramsUs x = gallonsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let gallonsUkToTeaspoonsUs x = gallonsUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let gallonsUkToTablespoonsUs x = gallonsUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let gallonsUkToFluidOuncesUs x = gallonsUkToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let gallonsUkToShotsUs x = gallonsUkToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let gallonsUkToGillsUs x = gallonsUkToShotsUs x |> shotsUsToGillsUs
        let gallonsUkToCupsUs x = gallonsUkToGillsUs x |> gillsUsToCupsUs
        let gallonsUkToPintsUs x = gallonsUkToCupsUs x |> cupsUsToPintsUs
        let gallonsUkToQuartsUs x = gallonsUkToPintsUs x |> pintsUsToQuartsUs
        let gallonsUkToPottlesUs x = gallonsUkToQuartsUs x |> quartsUsToPottlesUs
        let gallonsUkToBarrelsUs x = gallonsUkToGallonsUs x |> gallonsUsToBarrelsUs
        let gallonsUkToHogsheadsUs x = gallonsUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let gallonsUkToCubicInches x = gallonsUkToGallonsUs x |> gallonsUsToCubicInches
        let gallonsUkToMinimsUk x = gallonsUkToMinimsUs x |> minimsUsToMinimsUk
        let gallonsUkToFluidDrachmsUk x = gallonsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let gallonsUkToFluidOuncesUk x = gallonsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let gallonsUkToGillsUk x = gallonsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let gallonsUkToPintsUk x = gallonsUkToGillsUk x |> gillsUkToPintsUk
        let gallonsUkToBushelsUk x = gallonsUkToPecksUk x |> pecksUkToBushelsUk
        let gallonsUkToQuartersUk x = gallonsUkToBushelsUk x |> bushelsUkToQuartersUk
        let gallonsUkToMilliliters x = gallonsUkToLiters x |> litersToMilliliters

        let cubicInchesToMinimsUs x = cubicInchesToGallonsUs x |> gallonsUsToMinimsUs
        let cubicInchesToFluidDramsUs x = cubicInchesToMinimsUs x |> minimsUsToFluidDramsUs
        let cubicInchesToTeaspoonsUs x = cubicInchesToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let cubicInchesToTablespoonsUs x = cubicInchesToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let cubicInchesToFluidOuncesUs x = cubicInchesToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let cubicInchesToShotsUs x = cubicInchesToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let cubicInchesToGillsUs x = cubicInchesToShotsUs x |> shotsUsToGillsUs
        let cubicInchesToCupsUs x = cubicInchesToGillsUs x |> gillsUsToCupsUs
        let cubicInchesToPintsUs x = cubicInchesToCupsUs x |> cupsUsToPintsUs
        let cubicInchesToQuartsUs x = cubicInchesToPintsUs x |> pintsUsToQuartsUs
        let cubicInchesToPottlesUs x = cubicInchesToQuartsUs x |> quartsUsToPottlesUs
        let cubicInchesToBarrelsUs x = cubicInchesToGallonsUs x |> gallonsUsToBarrelsUs
        let cubicInchesToHogsheadsUs x = cubicInchesToBarrelsUs x |> barrelsUsToHogsheadsUs
        let cubicInchesToMinimsUk x = cubicInchesToMinimsUs x |> minimsUsToMinimsUk
        let cubicInchesToFluidDrachmsUk x = cubicInchesToMinimsUk x |> minimsUkToFluidDrachmsUk
        let cubicInchesToFluidOuncesUk x = cubicInchesToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let cubicInchesToGillsUk x = cubicInchesToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let cubicInchesToPintsUk x = cubicInchesToGillsUk x |> gillsUkToPintsUk
        let cubicInchesToQuartsUk x = cubicInchesToPintsUk x |> pintsUkToQuartsUk
        let cubicInchesToGallonsUk x = cubicInchesToQuartsUk x |> quartsUkToGallonsUk
        let cubicInchesToPecksUk x = cubicInchesToGallonsUk x |> gallonsUkToPecksUk
        let cubicInchesToBushelsUk x = cubicInchesToPecksUk x |> pecksUkToBushelsUk
        let cubicInchesToQuartersUk x = cubicInchesToBushelsUk x |> bushelsUkToQuartersUk
        let cubicInchesToMilliliters x = cubicInchesToMinimsUs x |> minimsUsToMilliliters
        let cubicInchesToLiters x = cubicInchesToMilliliters x |> millilitersToLiters

        let barrelsUsToMinimsUs x = barrelsUsToGallonsUs x |> gallonsUsToMinimsUs
        let barrelsUsToFluidDramsUs x = barrelsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let barrelsUsToTeaspoonsUs x = barrelsUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let barrelsUsToTablespoonsUs x = barrelsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let barrelsUsToFluidOuncesUs x = barrelsUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let barrelsUsToShotsUs x = barrelsUsToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let barrelsUsToGillsUs x = barrelsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let barrelsUsToCupsUs x = barrelsUsToGillsUs x |> gillsUsToCupsUs
        let barrelsUsToPintsUs x = barrelsUsToCupsUs x |> cupsUsToPintsUs
        let barrelsUsToQuartsUs x = barrelsUsToPintsUs x |> pintsUsToQuartsUs
        let barrelsUsToPottlesUs x = barrelsUsToQuartsUs x |> quartsUsToPottlesUs
        let barrelsUsToCubicInches x = barrelsUsToGallonsUs x |> gallonsUsToCubicInches
        let barrelsUsToMinimsUk x = barrelsUsToMinimsUs x |> minimsUsToMinimsUk
        let barrelsUsToFluidDrachmsUk x = barrelsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let barrelsUsToFluidOuncesUk x = barrelsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let barrelsUsToGillsUk x = barrelsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let barrelsUsToPintsUk x = barrelsUsToGillsUk x |> gillsUkToPintsUk
        let barrelsUsToQuartsUk x = barrelsUsToPintsUk x |> pintsUkToQuartsUk
        let barrelsUsToGallonsUk x = barrelsUsToQuartsUk x |> quartsUkToGallonsUk
        let barrelsUsToPecksUk x = barrelsUsToGallonsUk x |> gallonsUkToPecksUk
        let barrelsUsToBushelsUk x = barrelsUsToPecksUk x |> pecksUkToBushelsUk
        let barrelsUsToQuartersUk x = barrelsUsToBushelsUk x |> bushelsUkToQuartersUk
        let barrelsUsToMilliliters x = barrelsUsToGallonsUs x |> gallonsUsToMilliliters
        let barrelsUsToLiters x = barrelsUsToGallonsUs x |> gallonsUsToLiters

        let hogsheadsUsToMinimsUs x = hogsheadsUsToBarrelsUs x |> barrelsUsToMinimsUs
        let hogsheadsUsToFluidDramsUs x = hogsheadsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let hogsheadsUsToTeaspoonsUs x = hogsheadsUsToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let hogsheadsUsToTablespoonsUs x = hogsheadsUsToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let hogsheadsUsToFluidOuncesUs x = hogsheadsUsToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let hogsheadsUsToShotsUs x = hogsheadsUsToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let hogsheadsUsToGillsUs x = hogsheadsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let hogsheadsUsToCupsUs x = hogsheadsUsToGillsUs x |> gillsUsToCupsUs
        let hogsheadsUsToPintsUs x = hogsheadsUsToCupsUs x |> cupsUsToPintsUs
        let hogsheadsUsToQuartsUs x = hogsheadsUsToPintsUs x |> pintsUsToQuartsUs
        let hogsheadsUsToPottlesUs x = hogsheadsUsToQuartsUs x |> quartsUsToPottlesUs
        let hogsheadsUsToGallonsUs x = hogsheadsUsToPottlesUs x |> pottlesUsToGallonsUs
        let hogsheadsUsToCubicInches x = hogsheadsUsToGallonsUs x |> gallonsUsToCubicInches
        let hogsheadsUsToMinimsUk x = hogsheadsUsToMinimsUs x |> minimsUsToMinimsUk
        let hogsheadsUsToFluidDrachmsUk x = hogsheadsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let hogsheadsUsToFluidOuncesUk x = hogsheadsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let hogsheadsUsToGillsUk x = hogsheadsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let hogsheadsUsToPintsUk x = hogsheadsUsToGillsUk x |> gillsUkToPintsUk
        let hogsheadsUsToQuartsUk x = hogsheadsUsToPintsUk x |> pintsUkToQuartsUk
        let hogsheadsUsToGallonsUk x = hogsheadsUsToQuartsUk x |> quartsUkToGallonsUk
        let hogsheadsUsToPecksUk x = hogsheadsUsToGallonsUk x |> gallonsUkToPecksUk
        let hogsheadsUsToBushelsUk x = hogsheadsUsToPecksUk x |> pecksUkToBushelsUk
        let hogsheadsUsToQuartersUk x = hogsheadsUsToBushelsUk x |> bushelsUkToQuartersUk
        let hogsheadsUsToMilliliters x = hogsheadsUsToMinimsUs x |> minimsUsToMilliliters
        let hogsheadsUsToLiters x = hogsheadsUsToMinimsUs x |> minimsUsToLiters

        let pecksUkToMinimsUk x = pecksUkToGallonsUk x |> gallonsUkToMinimsUk
        let pecksUkToMinimsUs x = pecksUkToMinimsUk x |> minimsUkToMinimsUs
        let pecksUkToFluidDramsUs x = pecksUkToMinimsUs x |> minimsUsToFluidDramsUs
        let pecksUkToTeaspoonsUs x = pecksUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let pecksUkToTablespoonsUs x = pecksUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let pecksUkToFluidOuncesUs x = pecksUkToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let pecksUkToShotsUs x = pecksUkToTablespoonsUs x |> tablespoonsUsToShotsUs
        let pecksUkToGillsUs x = pecksUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let pecksUkToCupsUs x = pecksUkToGillsUs x |> gillsUsToCupsUs
        let pecksUkToPintsUs x = pecksUkToCupsUs x |> cupsUsToPintsUs
        let pecksUkToQuartsUs x = pecksUkToPintsUs x |> pintsUsToQuartsUs
        let pecksUkToPottlesUs x = pecksUkToQuartsUs x |> quartsUsToPottlesUs
        let pecksUkToGallonsUs x = pecksUkToPottlesUs x |> pottlesUsToGallonsUs
        let pecksUkToBarrelsUs x = pecksUkToGallonsUs x |> gallonsUsToBarrelsUs
        let pecksUkToHogsheadsUs x = pecksUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let pecksUkToCubicInches x = pecksUkToGallonsUs x |> gallonsUsToCubicInches
        let pecksUkToFluidDrachmsUk x = pecksUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let pecksUkToFluidOuncesUk x = pecksUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let pecksUkToGillsUk x = pecksUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let pecksUkToPintsUk x = pecksUkToGillsUk x |> gillsUkToPintsUk
        let pecksUkToQuartsUk x = pecksUkToPintsUk x |> pintsUkToQuartsUk
        let pecksUkToQuartersUk x = pecksUkToBushelsUk x |> bushelsUkToQuartersUk
        let pecksUkToMilliliters x = pecksUkToMinimsUs x |> minimsUsToMilliliters
        let pecksUkToLiters x = pecksUkToMinimsUs x |> minimsUsToLiters

        let bushelsUkToMinimsUk x = bushelsUkToPecksUk x |> pecksUkToMinimsUk
        let bushelsUkToMinimsUs x = bushelsUkToPecksUk x |> pecksUkToMinimsUs
        let bushelsUkToFluidDramsUs x = bushelsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let bushelsUkToTeaspoonsUs x = bushelsUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let bushelsUkToTablespoonsUs x = bushelsUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let bushelsUkToFluidOuncesUs x = bushelsUkToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let bushelsUkToShotsUs x = bushelsUkToTablespoonsUs x |> tablespoonsUsToShotsUs
        let bushelsUkToGillsUs x = bushelsUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let bushelsUkToCupsUs x = bushelsUkToGillsUs x |> gillsUsToCupsUs
        let bushelsUkToPintsUs x = bushelsUkToCupsUs x |> cupsUsToPintsUs
        let bushelsUkToQuartsUs x = bushelsUkToPintsUs x |> pintsUsToQuartsUs
        let bushelsUkToPottlesUs x = bushelsUkToQuartsUs x |> quartsUsToPottlesUs
        let bushelsUkToGallonsUs x = bushelsUkToPottlesUs x |> pottlesUsToGallonsUs
        let bushelsUkToBarrelsUs x = bushelsUkToGallonsUs x |> gallonsUsToBarrelsUs
        let bushelsUkToHogsheadsUs x = bushelsUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let bushelsUkToCubicInches x = bushelsUkToGallonsUs x |> gallonsUsToCubicInches
        let bushelsUkToFluidDrachmsUk x = bushelsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let bushelsUkToFluidOuncesUk x = bushelsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let bushelsUkToGillsUk x = bushelsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let bushelsUkToPintsUk x = bushelsUkToGillsUk x |> gillsUkToPintsUk
        let bushelsUkToQuartsUk x = bushelsUkToPintsUk x |> pintsUkToQuartsUk
        let bushelsUkToGallonsUk x = bushelsUkToQuartsUk x |> quartsUkToGallonsUk
        let bushelsUkToMilliliters x = bushelsUkToMinimsUk x |> minimsUkToMilliliters
        let bushelsUkToLiters x = bushelsUkToMinimsUk x |> minimsUkToLiters

        let quartersUkToMinimsUk x = quartersUkToBushelsUk x |> bushelsUkToMinimsUk
        let quartersUkToMinimsUs x = quartersUkToMinimsUk x |> minimsUkToMinimsUs
        let quartersUkToFluidDramsUs x = quartersUkToMinimsUs x |> minimsUsToFluidDramsUs
        let quartersUkToTeaspoonsUs x = quartersUkToFluidDramsUs x |> fluidDramsUsToTeaspoonsUs
        let quartersUkToTablespoonsUs x = quartersUkToTeaspoonsUs x |> teaspoonsUsToTablespoons
        let quartersUkToFluidOuncesUs x = quartersUkToTablespoonsUs x |> tablespoonsUsToFluidOuncesUs
        let quartersUkToShotsUs x = quartersUkToFluidOuncesUs x |> fluidOuncesUsToShotsUs
        let quartersUkToGillsUs x = quartersUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let quartersUkToCupsUs x = quartersUkToGillsUs x |> gillsUsToCupsUs
        let quartersUkToPintsUs x = quartersUkToCupsUs x |> cupsUsToPintsUs
        let quartersUkToQuartsUs x = quartersUkToPintsUs x |> pintsUsToQuartsUs
        let quartersUkToPottlesUs x = quartersUkToQuartsUs x |> quartsUsToPottlesUs
        let quartersUkToGallonsUs x = quartersUkToPottlesUs x |> pottlesUsToGallonsUs
        let quartersUkToBarrelsUs x = quartersUkToGallonsUs x |> gallonsUsToBarrelsUs
        let quartersUkToHogsheadsUs x = quartersUkToBarrelsUs x |> barrelsUsToHogsheadsUs
        let quartersUkToCubicInches x = quartersUkToGallonsUs x |> gallonsUsToCubicInches
        let quartersUkToFluidDrachmsUk x = quartersUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let quartersUkToFluidOuncesUk x = quartersUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let quartersUkToGillsUk x = quartersUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let quartersUkToPintsUk x = quartersUkToGillsUk x |> gillsUkToPintsUk
        let quartersUkToQuartsUk x = quartersUkToPintsUk x |> pintsUkToQuartsUk
        let quartersUkToGallonsUk x = quartersUkToQuartsUk x |> quartsUkToGallonsUk
        let quartersUkToPecksUk x = quartersUkToGallonsUk x |> gallonsUkToPecksUk
        let quartersUkToMilliliters x = quartersUkToMinimsUk x |> minimsUkToMilliliters
        let quartersUkToLiters x = quartersUkToMinimsUk x |> minimsUkToLiters

        type VolumeUnit = { amount: decimal; unitName: string}

        let packageVolumeUnit (x,y) = {amount = x; unitName = y}
        let roundVolume (x:decimal,y:string) = (System.Math.Round(x,0),y)
        let removeUnit (x:decimal<_>) = decimal x

        let convertVolumeVerified ((x:decimal),fromUnit,toUnit) =
            match (fromUnit,toUnit) with
            | ("milliliters","liters") -> {amount = millilitersToLiters(x*1m<milliliter>) |> removeUnit; unitName = "liters"}
            | ("milliliters","minimsUs") -> {amount = millilitersToMinimsUs(x*1m<milliliter>) |> removeUnit; unitName = "minimsUs"}
            | ("milliliters","fluidDramsUs") -> {amount = millilitersToFluidDramsUs(x*1m<milliliter>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("milliliters","teaspoonsUs") -> {amount = millilitersToTeaspoonsUs(x*1m<milliliter>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("milliliters","tablespoonsUs") -> {amount = millilitersToTablespoonsUs(x*1m<milliliter>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("milliliters","shotsUs") -> {amount = millilitersToShotsUs(x*1m<milliliter>) |> removeUnit; unitName = "shotsUs"}
            | ("milliliters","fluidOuncesUs") -> {amount = millilitersToFluidOuncesUs(x*1m<milliliter>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("milliliters","gillsUs") -> {amount = millilitersToGillsUs(x*1m<milliliter>) |> removeUnit; unitName = "gillsUs"}
            | ("milliliters","cupsUs") -> {amount = millilitersToCupsUs(x*1m<milliliter>) |> removeUnit; unitName = "cupsUs"}
            | ("milliliters","pintsUs") -> {amount = millilitersToPintsUs(x*1m<milliliter>) |> removeUnit; unitName = "pintsUs"}
            | ("milliliters","quartsUs") -> {amount = millilitersToQuartsUs(x*1m<milliliter>) |> removeUnit; unitName = "quartsUs"}
            | ("milliliters","pottlesUs") -> {amount = millilitersToPottlesUs(x*1m<milliliter>) |> removeUnit; unitName = "pottlesUs"}
            | ("milliliters","gallonsUs") -> {amount = millilitersToGallonsUs(x*1m<milliliter>) |> removeUnit; unitName = "gallonsUs"}
            | ("milliliters","barrelsUs") -> {amount = millilitersToBarrelsUs(x*1m<milliliter>) |> removeUnit; unitName = "barrelsUs"}
            | ("milliliters","hogsheadsUs") -> {amount = millilitersToHogsheadsUs(x*1m<milliliter>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("milliliters","cubicInches") -> {amount = millilitersToCubicInches(x*1m<milliliter>) |> removeUnit; unitName = "cubicInches"}
            | ("milliliters","minimsUk") -> {amount = millilitersToMinimsUk(x*1m<milliliter>) |> removeUnit; unitName = "minimsUk"}
            | ("milliliters","fluidDrachmsUk") -> {amount = millilitersToFluidDrachmsUk(x*1m<milliliter>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("milliliters","fluidOuncesUk") -> {amount = millilitersToFluidOuncesUk(x*1m<milliliter>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("milliliters","gillsUk") -> {amount = millilitersToGillsUk(x*1m<milliliter>) |> removeUnit; unitName = "gillsUk"}
            | ("milliliters","pintsUk") -> {amount = millilitersToPintsUk(x*1m<milliliter>) |> removeUnit; unitName = "pintsUk"}
            | ("milliliters","quartsUk") -> {amount = millilitersToQuartsUk(x*1m<milliliter>) |> removeUnit; unitName = "quartsUk"}
            | ("milliliters","gallonsUk") -> {amount = millilitersToGallonsUk(x*1m<milliliter>) |> removeUnit; unitName = "gallonsUk"}
            | ("milliliters","pecksUk") -> {amount = millilitersToPecksUk(x*1m<milliliter>) |> removeUnit; unitName = "pecksUk"}
            | ("milliliters","bushelsUk") -> {amount = millilitersToBushelsUk(x*1m<milliliter>) |> removeUnit; unitName = "bushelsUk"}
            | ("milliliters","quartersUk") -> {amount = millilitersToQuartersUk(x*1m<milliliter>) |> removeUnit; unitName = "quartersUk"}           
            | ("liters","milliliters") -> {amount = litersToMilliliters(x*1m<liter>) |> removeUnit; unitName = "milliliters"}
            | ("liters","minimsUs") -> {amount = litersToMinimsUs(x*1m<liter>) |> removeUnit; unitName = "minimsUs"}
            | ("liters","fluidDramsUs") -> {amount = litersToFluidDramsUs(x*1m<liter>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("liters","teaspoonsUs") -> {amount = litersToTeaspoonsUs(x*1m<liter>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("liters","tablespoonsUs") -> {amount = litersToTablespoonsUs(x*1m<liter>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("liters","shotsUs") -> {amount = litersToShotsUs(x*1m<liter>) |> removeUnit; unitName = "shotsUs"}
            | ("liters","fluidOuncesUs") -> {amount = litersToFluidOuncesUs(x*1m<liter>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("liters","gillsUs") -> {amount = litersToGillsUs(x*1m<liter>) |> removeUnit; unitName = "gillsUs"}
            | ("liters","cupsUs") -> {amount = litersToCupsUs(x*1m<liter>) |> removeUnit; unitName = "cupsUs"}
            | ("liters","pintsUs") -> {amount = litersToPintsUs(x*1m<liter>) |> removeUnit; unitName = "pintsUs"}
            | ("liters","quartsUs") -> {amount = litersToQuartsUs(x*1m<liter>) |> removeUnit; unitName = "quartsUs"}
            | ("liters","pottlesUs") -> {amount = litersToPottlesUs(x*1m<liter>) |> removeUnit; unitName = "pottlesUs"}
            | ("liters","gallonsUs") -> {amount = litersToGallonsUs(x*1m<liter>) |> removeUnit; unitName = "gallonsUs"}
            | ("liters","barrelsUs") -> {amount = litersToBarrelsUs(x*1m<liter>) |> removeUnit; unitName = "barrelsUs"}
            | ("liters","hogsheadsUs") -> {amount = litersToHogsheadsUs(x*1m<liter>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("liters","cubicInches") -> {amount = litersToCubicInches(x*1m<liter>) |> removeUnit; unitName = "cubicInches"}
            | ("liters","minimsUk") -> {amount = litersToMinimsUk(x*1m<liter>) |> removeUnit; unitName = "minimsUk"}
            | ("liters","fluidDrachmsUk") -> {amount = litersToFluidDrachmsUk(x*1m<liter>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("liters","fluidOuncesUk") -> {amount = litersToFluidOuncesUk(x*1m<liter>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("liters","gillsUk") -> {amount = litersToGillsUk(x*1m<liter>) |> removeUnit; unitName = "gillsUk"}
            | ("liters","pintsUk") -> {amount = litersToPintsUk(x*1m<liter>) |> removeUnit; unitName = "pintsUk"}
            | ("liters","quartsUk") -> {amount = litersToQuartsUk(x*1m<liter>) |> removeUnit; unitName = "quartsUk"}
            | ("liters","gallonsUk") -> {amount = litersToGallonsUk(x*1m<liter>) |> removeUnit; unitName = "gallonsUk"}
            | ("liters","pecksUk") -> {amount = litersToPecksUk(x*1m<liter>) |> removeUnit; unitName = "pecksUk"}
            | ("liters","bushelsUk") -> {amount = litersToBushelsUk(x*1m<liter>) |> removeUnit; unitName = "bushelsUk"}
            | ("liters","quartersUk") -> {amount = litersToQuartersUk(x*1m<liter>) |> removeUnit; unitName = "quartersUk"} 
            | ("minimsUs","milliliters") -> {amount = minimsUsToMilliliters(x*1m<minim_us>) |> removeUnit; unitName = "milliliters"}
            | ("minimsUs","liters") -> {amount = minimsUsToLiters(x*1m<minim_us>) |> removeUnit; unitName = "liters"}
            | ("minimsUs","fluidDramsUs") -> {amount = minimsUsToFluidDramsUs(x*1m<minim_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("minimsUs","teaspoonsUs") -> {amount = minimsUsToTeaspoonsUs(x*1m<minim_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("minimsUs","tablespoonsUs") -> {amount = minimsUsToTablespoonsUs(x*1m<minim_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("minimsUs","shotsUs") -> {amount = minimsUsToShotsUs(x*1m<minim_us>) |> removeUnit; unitName = "shotsUs"}
            | ("minimsUs","fluidOuncesUs") -> {amount = minimsUsToFluidOuncesUs(x*1m<minim_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("minimsUs","gillsUs") -> {amount = minimsUsToGillsUs(x*1m<minim_us>) |> removeUnit; unitName = "gillsUs"}
            | ("minimsUs","cupsUs") -> {amount = minimsUsToCupsUs(x*1m<minim_us>) |> removeUnit; unitName = "cupsUs"}
            | ("minimsUs","pintsUs") -> {amount = minimsUsToPintsUs(x*1m<minim_us>) |> removeUnit; unitName = "pintsUs"}
            | ("minimsUs","quartsUs") -> {amount = minimsUsToQuartsUs(x*1m<minim_us>) |> removeUnit; unitName = "quartsUs"}
            | ("minimsUs","pottlesUs") -> {amount = minimsUsToPottlesUs(x*1m<minim_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("minimsUs","gallonsUs") -> {amount = minimsUsToGallonsUs(x*1m<minim_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("minimsUs","barrelsUs") -> {amount = minimsUsToBarrelsUs(x*1m<minim_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("minimsUs","hogsheadsUs") -> {amount = minimsUsToHogsheadsUs(x*1m<minim_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("minimsUs","cubicInches") -> {amount = minimsUsToCubicInches(x*1m<minim_us>) |> removeUnit; unitName = "cubicInches"}
            | ("minimsUs","minimsUk") -> {amount = minimsUsToMinimsUk(x*1m<minim_us>) |> removeUnit; unitName = "minimsUk"}
            | ("minimsUs","fluidDrachmsUk") -> {amount = minimsUsToFluidDrachmsUk(x*1m<minim_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("minimsUs","fluidOuncesUk") -> {amount = minimsUsToFluidOuncesUk(x*1m<minim_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("minimsUs","gillsUk") -> {amount = minimsUsToGillsUk(x*1m<minim_us>) |> removeUnit; unitName = "gillsUk"}
            | ("minimsUs","pintsUk") -> {amount = minimsUsToPintsUk(x*1m<minim_us>) |> removeUnit; unitName = "pintsUk"}
            | ("minimsUs","quartsUk") -> {amount = minimsUsToQuartsUk(x*1m<minim_us>) |> removeUnit; unitName = "quartsUk"}
            | ("minimsUs","gallonsUk") -> {amount = minimsUsToGallonsUk(x*1m<minim_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("minimsUs","pecksUk") -> {amount = minimsUsToPecksUk(x*1m<minim_us>) |> removeUnit; unitName = "pecksUk"}
            | ("minimsUs","bushelsUk") -> {amount = minimsUsToBushelsUk(x*1m<minim_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("minimsUs","quartersUk") -> {amount = minimsUsToQuartersUk(x*1m<minim_us>) |> removeUnit; unitName = "quartersUk"}
            | ("fluidDramsUs","milliliters") -> {amount = fluidDramsUsToMilliliters(x*1m<fluidDram_us>) |> removeUnit; unitName = "milliliters"}
            | ("fluidDramsUs","liters") -> {amount = fluidDramsUsToLiters(x*1m<fluidDram_us>) |> removeUnit; unitName = "liters"}
            | ("fluidDramsUs","minimsUs") -> {amount = fluidDramsUsToMinimsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "minimsUs"}
            | ("fluidDramsUs","teaspoonsUs") -> {amount = fluidDramsUsToTeaspoonsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("fluidDramsUs","tablespoonsUs") -> {amount = fluidDramsUsToTablespoonsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("fluidDramsUs","shotsUs") -> {amount = fluidDramsUsToShotsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "shotsUs"}
            | ("fluidDramsUs","fluidOuncesUs") -> {amount = fluidDramsUsToFluidOuncesUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("fluidDramsUs","gillsUs") -> {amount = fluidDramsUsToGillsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "gillsUs"}
            | ("fluidDramsUs","cupsUs") -> {amount = fluidDramsUsToCupsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "cupsUs"}
            | ("fluidDramsUs","pintsUs") -> {amount = fluidDramsUsToPintsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "pintsUs"}
            | ("fluidDramsUs","quartsUs") -> {amount = fluidDramsUsToQuartsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "quartsUs"}
            | ("fluidDramsUs","pottlesUs") -> {amount = fluidDramsUsToPottlesUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("fluidDramsUs","gallonsUs") -> {amount = fluidDramsUsToGallonsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("fluidDramsUs","barrelsUs") -> {amount = fluidDramsUsToBarrelsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("fluidDramsUs","hogsheadsUs") -> {amount = fluidDramsUsToHogsheadsUs(x*1m<fluidDram_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("fluidDramsUs","cubicInches") -> {amount = fluidDramsUsToCubicInches(x*1m<fluidDram_us>) |> removeUnit; unitName = "cubicInches"}
            | ("fluidDramsUs","minimsUk") -> {amount = fluidDramsUsToMinimsUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "minimsUk"}
            | ("fluidDramsUs","fluidDrachmsUk") -> {amount = fluidDramsUsToFluidDrachmsUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("fluidDramsUs","fluidOuncesUk") -> {amount = fluidDramsUsToFluidOuncesUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("fluidDramsUs","gillsUk") -> {amount = fluidDramsUsToGillsUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "gillsUk"}
            | ("fluidDramsUs","pintsUk") -> {amount = fluidDramsUsToPintsUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "pintsUk"}
            | ("fluidDramsUs","quartsUk") -> {amount = fluidDramsUsToQuartsUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "quartsUk"}
            | ("fluidDramsUs","gallonsUk") -> {amount = fluidDramsUsToGallonsUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("fluidDramsUs","pecksUk") -> {amount = fluidDramsUsToPecksUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "pecksUk"}
            | ("fluidDramsUs","bushelsUk") -> {amount = fluidDramsUsToBushelsUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("fluidDramsUs","quartersUk") -> {amount = fluidDramsUsToQuartersUk(x*1m<fluidDram_us>) |> removeUnit; unitName = "quartersUk"}            
            | ("teaspoonsUs","milliliters") -> {amount = teaspoonsUsToMilliliters(x*1m<teaspoon_us>) |> removeUnit; unitName = "milliliters"}
            | ("teaspoonsUs","liters") -> {amount = teaspoonsUsToLiters(x*1m<teaspoon_us>) |> removeUnit; unitName = "liters"}
            | ("teaspoonsUs","minimsUs") -> {amount = teaspoonsUsToMinimsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "minimsUs"}
            | ("teaspoonsUs","fluidDramsUs") -> {amount = teaspoonsUsToFluidDramsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("teaspoonsUs","tablespoonsUs") -> {amount = teaspoonsUsToTablespoons(x*1m<teaspoon_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("teaspoonsUs","shotsUs") -> {amount = teaspoonsUsToShotsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "shotsUs"}
            | ("teaspoonsUs","fluidOuncesUs") -> {amount = teaspoonsUsToFluidOuncesUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("teaspoonsUs","gillsUs") -> {amount = teaspoonsUsToGillsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "gillsUs"}
            | ("teaspoonsUs","cupsUs") -> {amount = teaspoonsUsToCupsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "cupsUs"}
            | ("teaspoonsUs","pintsUs") -> {amount = teaspoonsUsToPintsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "pintsUs"}
            | ("teaspoonsUs","quartsUs") -> {amount = teaspoonsUsToQuartsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "quartsUs"}
            | ("teaspoonsUs","pottlesUs") -> {amount = teaspoonsUsToPottlesUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("teaspoonsUs","gallonsUs") -> {amount = teaspoonsUsToGallonsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("teaspoonsUs","barrelsUs") -> {amount = teaspoonsUsToBarrelsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("teaspoonsUs","hogsheadsUs") -> {amount = teaspoonsUsToHogsheadsUs(x*1m<teaspoon_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("teaspoonsUs","cubicInches") -> {amount = teaspoonsUsToCubicInches(x*1m<teaspoon_us>) |> removeUnit; unitName = "cubicInches"}
            | ("teaspoonsUs","minimsUk") -> {amount = teaspoonsUsToMinimsUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "minimsUk"}
            | ("teaspoonsUs","fluidDrachmsUk") -> {amount = teaspoonsUsToFluidDrachmsUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("teaspoonsUs","fluidOuncesUk") -> {amount = teaspoonsUsToFluidOuncesUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("teaspoonsUs","gillsUk") -> {amount = teaspoonsUsToGillsUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "gillsUk"}
            | ("teaspoonsUs","pintsUk") -> {amount = teaspoonsUsToPintsUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "pintsUk"}
            | ("teaspoonsUs","quartsUk") -> {amount = teaspoonsUsToQuartsUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "quartsUk"}
            | ("teaspoonsUs","gallonsUk") -> {amount = teaspoonsUsToGallonsUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("teaspoonsUs","pecksUk") -> {amount = teaspoonsUsToPecksUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "pecksUk"}
            | ("teaspoonsUs","bushelsUk") -> {amount = teaspoonsUsToBushelsUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("teaspoonsUs","quartersUk") -> {amount = teaspoonsUsToQuartersUk(x*1m<teaspoon_us>) |> removeUnit; unitName = "quartersUk"}
            | ("tablespoonsUs","milliliters") -> {amount = tablespoonsUsToMilliliters(x*1m<tablespoon_us>) |> removeUnit; unitName = "milliliters"}
            | ("tablespoonsUs","liters") -> {amount = tablespoonsUsToLiters(x*1m<tablespoon_us>) |> removeUnit; unitName = "liters"}
            | ("tablespoonsUs","minimsUs") -> {amount = tablespoonsUsToMinimsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "minimsUs"}
            | ("tablespoonsUs","fluidDramsUs") -> {amount = tablespoonsUsToFluidDramsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("tablespoonsUs","teaspoonsUs") -> {amount = tablespoonsUsToTeaspoonsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("tablespoonsUs","shotsUs") -> {amount = tablespoonsUsToShotsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "shotsUs"}
            | ("tablespoonsUs","fluidOuncesUs") -> {amount = tablespoonsUsToFluidOuncesUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("tablespoonsUs","gillsUs") -> {amount = tablespoonsUsToGillsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "gillsUs"}
            | ("tablespoonsUs","cupsUs") -> {amount = tablespoonsUsToCupsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "cupsUs"}
            | ("tablespoonsUs","pintsUs") -> {amount = tablespoonsUsToPintsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "pintsUs"}
            | ("tablespoonsUs","quartsUs") -> {amount = tablespoonsUsToQuartsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "quartsUs"}
            | ("tablespoonsUs","pottlesUs") -> {amount = tablespoonsUsToPottlesUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("tablespoonsUs","gallonsUs") -> {amount = tablespoonsUsToGallonsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("tablespoonsUs","barrelsUs") -> {amount = tablespoonsUsToBarrelsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("tablespoonsUs","hogsheadsUs") -> {amount = tablespoonsUsToHogsheadsUs(x*1m<tablespoon_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("tablespoonsUs","cubicInches") -> {amount = tablespoonsUsToCubicInches(x*1m<tablespoon_us>) |> removeUnit; unitName = "cubicInches"}
            | ("tablespoonsUs","minimsUk") -> {amount = tablespoonsUsToMinimsUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "minimsUk"}
            | ("tablespoonsUs","fluidDrachmsUk") -> {amount = tablespoonsUsToFluidDrachmsUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("tablespoonsUs","fluidOuncesUk") -> {amount = tablespoonsUsToFluidOuncesUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("tablespoonsUs","gillsUk") -> {amount = tablespoonsUsToGillsUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "gillsUk"}
            | ("tablespoonsUs","pintsUk") -> {amount = tablespoonsUsToPintsUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "pintsUk"}
            | ("tablespoonsUs","quartsUk") -> {amount = tablespoonsUsToQuartsUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "quartsUk"}
            | ("tablespoonsUs","gallonsUk") -> {amount = tablespoonsUsToGallonsUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("tablespoonsUs","pecksUk") -> {amount = tablespoonsUsToPecksUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "pecksUk"}
            | ("tablespoonsUs","bushelsUk") -> {amount = tablespoonsUsToBushelsUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("tablespoonsUs","quartersUk") -> {amount = tablespoonsUsToQuartersUk(x*1m<tablespoon_us>) |> removeUnit; unitName = "quartersUk"}
            | ("fluidOuncesUs","milliliters") -> {amount = fluidOuncesUsToMilliliters(x*1m<fluidOunce_us>) |> removeUnit; unitName = "milliliters"}
            | ("fluidOuncesUs","liters") -> {amount = fluidOuncesUsToLiters(x*1m<fluidOunce_us>) |> removeUnit; unitName = "liters"}
            | ("fluidOuncesUs","minimsUs") -> {amount = fluidOuncesUsToMinimsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "minimsUs"}
            | ("fluidOuncesUs","fluidDramsUs") -> {amount = fluidOuncesUsToFluidDramsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("fluidOuncesUs","teaspoonsUs") -> {amount = fluidOuncesUsToTeaspoonsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("fluidOuncesUs","shotsUs") -> {amount = fluidOuncesUsToShotsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "shotsUs"}
            | ("fluidOuncesUs","tablespoonsUs") -> {amount = fluidOuncesUsToTablespoonsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("fluidOuncesUs","gillsUs") -> {amount = fluidOuncesUsToGillsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "gillsUs"}
            | ("fluidOuncesUs","cupsUs") -> {amount = fluidOuncesUsToCupsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "cupsUs"}
            | ("fluidOuncesUs","pintsUs") -> {amount = fluidOuncesUsToPintsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "pintsUs"}
            | ("fluidOuncesUs","quartsUs") -> {amount = fluidOuncesUsToQuartsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "quartsUs"}
            | ("fluidOuncesUs","pottlesUs") -> {amount = fluidOuncesUsToPottlesUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("fluidOuncesUs","gallonsUs") -> {amount = fluidOuncesUsToGallonsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("fluidOuncesUs","barrelsUs") -> {amount = fluidOuncesUsToBarrelsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("fluidOuncesUs","hogsheadsUs") -> {amount = fluidOuncesUsToHogsheadsUs(x*1m<fluidOunce_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("fluidOuncesUs","cubicInches") -> {amount = fluidOuncesUsToCubicInches(x*1m<fluidOunce_us>) |> removeUnit; unitName = "cubicInches"}
            | ("fluidOuncesUs","minimsUk") -> {amount = fluidOuncesUsToMinimsUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "minimsUk"}
            | ("fluidOuncesUs","fluidDrachmsUk") -> {amount = fluidOuncesUsToFluidDrachmsUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("fluidOuncesUs","fluidOuncesUk") -> {amount = fluidOuncesUsToFluidOuncesUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("fluidOuncesUs","gillsUk") -> {amount = fluidOuncesUsToGillsUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "gillsUk"}
            | ("fluidOuncesUs","pintsUk") -> {amount = fluidOuncesUsToPintsUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "pintsUk"}
            | ("fluidOuncesUs","quartsUk") -> {amount = fluidOuncesUsToQuartsUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "quartsUk"}
            | ("fluidOuncesUs","gallonsUk") -> {amount = fluidOuncesUsToGallonsUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("fluidOuncesUs","pecksUk") -> {amount = fluidOuncesUsToPecksUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "pecksUk"}
            | ("fluidOuncesUs","bushelsUk") -> {amount = fluidOuncesUsToBushelsUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("fluidOuncesUs","quartersUk") -> {amount = fluidOuncesUsToQuartersUk(x*1m<fluidOunce_us>) |> removeUnit; unitName = "quartersUk"}
            | ("shotsUs","milliliters") -> {amount = shotsUsToMilliliters(x*1m<shot_us>) |> removeUnit; unitName = "milliliters"}
            | ("shotsUs","liters") -> {amount = shotsUsToLiters(x*1m<shot_us>) |> removeUnit; unitName = "liters"}
            | ("shotsUs","minimsUs") -> {amount = shotsUsToMinimsUs(x*1m<shot_us>) |> removeUnit; unitName = "minimsUs"}
            | ("shotsUs","fluidDramsUs") -> {amount = shotsUsToFluidDramsUs(x*1m<shot_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("shotsUs","teaspoonsUs") -> {amount = shotsUsToTeaspoonsUs(x*1m<shot_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("shotsUs","fluidOuncesUs") -> {amount = shotsUsToFluidOuncesUs(x*1m<shot_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("shotsUs","tablespoonsUs") -> {amount = shotsToTablespoonsUs(x*1m<shot_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("shotsUs","gillsUs") -> {amount = shotsUsToGillsUs(x*1m<shot_us>) |> removeUnit; unitName = "gillsUs"}
            | ("shotsUs","cupsUs") -> {amount = shotsUsToCupsUs(x*1m<shot_us>) |> removeUnit; unitName = "cupsUs"}
            | ("shotsUs","pintsUs") -> {amount = shotsUsToPintsUs(x*1m<shot_us>) |> removeUnit; unitName = "pintsUs"}
            | ("shotsUs","quartsUs") -> {amount = shotsUsToQuartsUs(x*1m<shot_us>) |> removeUnit; unitName = "quartsUs"}
            | ("shotsUs","pottlesUs") -> {amount = shotsUsToPottlesUs(x*1m<shot_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("shotsUs","gallonsUs") -> {amount = shotsUsToGallonsUs(x*1m<shot_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("shotsUs","barrelsUs") -> {amount = shotsUsToBarrelsUs(x*1m<shot_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("shotsUs","hogsheadsUs") -> {amount = shotsUsToHogsheadsUs(x*1m<shot_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("shotsUs","cubicInches") -> {amount = shotsUsToCubicInches(x*1m<shot_us>) |> removeUnit; unitName = "cubicInches"}
            | ("shotsUs","minimsUk") -> {amount = shotsUsToMinimsUk(x*1m<shot_us>) |> removeUnit; unitName = "minimsUk"}
            | ("shotsUs","fluidDrachmsUk") -> {amount = shotsUsToFluidDrachmsUk(x*1m<shot_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("shotsUs","fluidOuncesUk") -> {amount = shotsUsToFluidOuncesUk(x*1m<shot_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("shotsUs","gillsUk") -> {amount = shotsUsToGillsUk(x*1m<shot_us>) |> removeUnit; unitName = "gillsUk"}
            | ("shotsUs","pintsUk") -> {amount = shotsUsToPintsUk(x*1m<shot_us>) |> removeUnit; unitName = "pintsUk"}
            | ("shotsUs","quartsUk") -> {amount = shotsUsToQuartsUk(x*1m<shot_us>) |> removeUnit; unitName = "quartsUk"}
            | ("shotsUs","gallonsUk") -> {amount = shotsUsToGallonsUk(x*1m<shot_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("shotsUs","pecksUk") -> {amount = shotsUsToPecksUk(x*1m<shot_us>) |> removeUnit; unitName = "pecksUk"}
            | ("shotsUs","bushelsUk") -> {amount = shotsUsToBushelsUk(x*1m<shot_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("shotsUs","quartersUk") -> {amount = shotsUsToQuartersUk(x*1m<shot_us>) |> removeUnit; unitName = "quartersUk"}
            | ("gillsUs","milliliters") -> {amount = gillsUsToMilliliters(x*1m<gill_us>) |> removeUnit; unitName = "milliliters"}
            | ("gillsUs","liters") -> {amount = gillsUsToLiters(x*1m<gill_us>) |> removeUnit; unitName = "liters"}
            | ("gillsUs","minimsUs") -> {amount = gillsUsToMinimsUs(x*1m<gill_us>) |> removeUnit; unitName = "minimsUs"}
            | ("gillsUs","fluidDramsUs") -> {amount = gillsUsToFluidDramsUs(x*1m<gill_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("gillsUs","teaspoonsUs") -> {amount = gillsUsToTeaspoonsUs(x*1m<gill_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("gillsUs","fluidOuncesUs") -> {amount = gillsUsToFluidOuncesUs(x*1m<gill_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("gillsUs","tablespoonsUs") -> {amount = gillsUsToTablespoonsUs(x*1m<gill_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("gillsUs","shotsUs") -> {amount = gillsUsToShotsUs(x*1m<gill_us>) |> removeUnit; unitName = "shotsUs"}
            | ("gillsUs","cupsUs") -> {amount = gillsUsToCupsUs(x*1m<gill_us>) |> removeUnit; unitName = "cupsUs"}
            | ("gillsUs","pintsUs") -> {amount = gillsUsToPintsUs(x*1m<gill_us>) |> removeUnit; unitName = "pintsUs"}
            | ("gillsUs","quartsUs") -> {amount = gillsUsToQuartsUs(x*1m<gill_us>) |> removeUnit; unitName = "quartsUs"}
            | ("gillsUs","pottlesUs") -> {amount = gillsUsToPottlesUs(x*1m<gill_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("gillsUs","gallonsUs") -> {amount = gillsUsToGallonsUs(x*1m<gill_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("gillsUs","barrelsUs") -> {amount = gillsUsToBarrelsUs(x*1m<gill_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("gillsUs","hogsheadsUs") -> {amount = gillsUsToHogsheadsUs(x*1m<gill_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("gillsUs","cubicInches") -> {amount = gillsUsToCubicInches(x*1m<gill_us>) |> removeUnit; unitName = "cubicInches"}
            | ("gillsUs","minimsUk") -> {amount = gillsUsToMinimsUk(x*1m<gill_us>) |> removeUnit; unitName = "minimsUk"}
            | ("gillsUs","fluidDrachmsUk") -> {amount = gillsUsToFluidDrachmsUk(x*1m<gill_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("gillsUs","fluidOuncesUk") -> {amount = gillsUsToFluidOuncesUk(x*1m<gill_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("gillsUs","gillsUk") -> {amount = gillsUsToGillsUk(x*1m<gill_us>) |> removeUnit; unitName = "gillsUk"}
            | ("gillsUs","pintsUk") -> {amount = gillsUsToPintsUk(x*1m<gill_us>) |> removeUnit; unitName = "pintsUk"}
            | ("gillsUs","quartsUk") -> {amount = gillsUsToQuartsUk(x*1m<gill_us>) |> removeUnit; unitName = "quartsUk"}
            | ("gillsUs","gallonsUk") -> {amount = gillsUsToGallonsUk(x*1m<gill_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("gillsUs","pecksUk") -> {amount = gillsUsToPecksUk(x*1m<gill_us>) |> removeUnit; unitName = "pecksUk"}
            | ("gillsUs","bushelsUk") -> {amount = gillsUsToBushelsUk(x*1m<gill_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("gillsUs","quartersUk") -> {amount = gillsUsToQuartersUk(x*1m<gill_us>) |> removeUnit; unitName = "quartersUk"}
            | ("cupsUs","milliliters") -> {amount = cupsUsToMilliliters(x*1m<cup_us>) |> removeUnit; unitName = "milliliters"}
            | ("cupsUs","liters") -> {amount = cupsUsToLiters(x*1m<cup_us>) |> removeUnit; unitName = "liters"}
            | ("cupsUs","minimsUs") -> {amount = cupsUsToMinimsUs(x*1m<cup_us>) |> removeUnit; unitName = "minimsUs"}
            | ("cupsUs","fluidDramsUs") -> {amount = cupsUsToFluidDramsUs(x*1m<cup_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("cupsUs","teaspoonsUs") -> {amount = cupsUsToTeaspoonsUs(x*1m<cup_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("cupsUs","fluidOuncesUs") -> {amount = cupsUsToFluidOuncesUs(x*1m<cup_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("cupsUs","tablespoonsUs") -> {amount = cupsUsToTablespoonsUs(x*1m<cup_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("cupsUs","shotsUs") -> {amount = cupsUsToShotsUs(x*1m<cup_us>) |> removeUnit; unitName = "shotsUs"}
            | ("cupsUs","gillsUs") -> {amount = cupsUsToGillsUs(x*1m<cup_us>) |> removeUnit; unitName = "gillsUs"}
            | ("cupsUs","pintsUs") -> {amount = cupsUsToPintsUs(x*1m<cup_us>) |> removeUnit; unitName = "pintsUs"}
            | ("cupsUs","quartsUs") -> {amount = cupsUsToQuartsUs(x*1m<cup_us>) |> removeUnit; unitName = "quartsUs"}
            | ("cupsUs","pottlesUs") -> {amount = cupsUsToPottlesUs(x*1m<cup_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("cupsUs","gallonsUs") -> {amount = cupsUsToGallonsUs(x*1m<cup_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("cupsUs","barrelsUs") -> {amount = cupsUsToBarrelsUs(x*1m<cup_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("cupsUs","hogsheadsUs") -> {amount = cupsUsToHogsheadsUs(x*1m<cup_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("cupsUs","cubicInches") -> {amount = cupsUsToCubicInches(x*1m<cup_us>) |> removeUnit; unitName = "cubicInches"}
            | ("cupsUs","minimsUk") -> {amount = cupsUsToMinimsUk(x*1m<cup_us>) |> removeUnit; unitName = "minimsUk"}
            | ("cupsUs","fluidDrachmsUk") -> {amount = cupsUsToFluidDrachmsUk(x*1m<cup_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("cupsUs","fluidOuncesUk") -> {amount = cupsUsToFluidOuncesUk(x*1m<cup_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("cupsUs","gillsUk") -> {amount = cupsUsToGillsUk(x*1m<cup_us>) |> removeUnit; unitName = "gillsUk"}
            | ("cupsUs","pintsUk") -> {amount = cupsUsToPintsUk(x*1m<cup_us>) |> removeUnit; unitName = "pintsUk"}
            | ("cupsUs","quartsUk") -> {amount = cupsUsToQuartsUk(x*1m<cup_us>) |> removeUnit; unitName = "quartsUk"}
            | ("cupsUs","gallonsUk") -> {amount = cupsUsToGallonsUk(x*1m<cup_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("cupsUs","pecksUk") -> {amount = cupsUsToPecksUk(x*1m<cup_us>) |> removeUnit; unitName = "pecksUk"}
            | ("cupsUs","bushelsUk") -> {amount = cupsUsToBushelsUk(x*1m<cup_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("cupsUs","quartersUk") -> {amount = cupsUsToQuartersUk(x*1m<cup_us>) |> removeUnit; unitName = "quartersUk"}
            | ("pintsUs","milliliters") -> {amount = pintsUsToMilliliters(x*1m<pint_us>) |> removeUnit; unitName = "milliliters"}
            | ("pintsUs","liters") -> {amount = pintsUsToLiters(x*1m<pint_us>) |> removeUnit; unitName = "liters"}
            | ("pintsUs","minimsUs") -> {amount = pintsUsToMinimsUs(x*1m<pint_us>) |> removeUnit; unitName = "minimsUs"}
            | ("pintsUs","fluidDramsUs") -> {amount = pintsUsToFluidDramsUs(x*1m<pint_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("pintsUs","teaspoonsUs") -> {amount = pintsUsToTeaspoonsUs(x*1m<pint_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("pintsUs","fluidOuncesUs") -> {amount = pintsUsToFluidOuncesUs(x*1m<pint_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("pintsUs","tablespoonsUs") -> {amount = pintsUsToTablespoonsUs(x*1m<pint_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("pintsUs","shotsUs") -> {amount = pintsUsToShotsUs(x*1m<pint_us>) |> removeUnit; unitName = "shotsUs"}
            | ("pintsUs","gillsUs") -> {amount = pintsUsToGillsUs(x*1m<pint_us>) |> removeUnit; unitName = "gillsUs"}
            | ("pintsUs","cupsUs") -> {amount = pintsUsToCupsUs(x*1m<pint_us>) |> removeUnit; unitName = "cupsUs"}
            | ("pintsUs","quartsUs") -> {amount = pintsUsToQuartsUs(x*1m<pint_us>) |> removeUnit; unitName = "quartsUs"}
            | ("pintsUs","pottlesUs") -> {amount = pintsUsToPottlesUs(x*1m<pint_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("pintsUs","gallonsUs") -> {amount = pintsUsToGallonsUs(x*1m<pint_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("pintsUs","barrelsUs") -> {amount = pintsUsToBarrelsUs(x*1m<pint_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("pintsUs","hogsheadsUs") -> {amount = pintsUsToHogsheadsUs(x*1m<pint_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("pintsUs","cubicInches") -> {amount = pintsUsToCubicInches(x*1m<pint_us>) |> removeUnit; unitName = "cubicInches"}
            | ("pintsUs","minimsUk") -> {amount = pintsUsToMinimsUk(x*1m<pint_us>) |> removeUnit; unitName = "minimsUk"}
            | ("pintsUs","fluidDrachmsUk") -> {amount = pintsUsToFluidDrachmsUk(x*1m<pint_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("pintsUs","fluidOuncesUk") -> {amount = pintsUsToFluidOuncesUk(x*1m<pint_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("pintsUs","gillsUk") -> {amount = pintsUsToGillsUk(x*1m<pint_us>) |> removeUnit; unitName = "gillsUk"}
            | ("pintsUs","pintsUk") -> {amount = pintsUsToPintsUk(x*1m<pint_us>) |> removeUnit; unitName = "pintsUk"}
            | ("pintsUs","quartsUk") -> {amount = pintsUsToQuartsUk(x*1m<pint_us>) |> removeUnit; unitName = "quartsUk"}
            | ("pintsUs","gallonsUk") -> {amount = pintsUsToGallonsUk(x*1m<pint_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("pintsUs","pecksUk") -> {amount = pintsUsToPecksUk(x*1m<pint_us>) |> removeUnit; unitName = "pecksUk"}
            | ("pintsUs","bushelsUk") -> {amount = pintsUsToBushelsUk(x*1m<pint_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("pintsUs","quartersUk") -> {amount = pintsUsToQuartersUk(x*1m<pint_us>) |> removeUnit; unitName = "quartersUk"}
            | ("quartsUs","milliliters") -> {amount = quartsUsToMilliliters(x*1m<quart_us>) |> removeUnit; unitName = "milliliters"}
            | ("quartsUs","liters") -> {amount = quartsUsToLiters(x*1m<quart_us>) |> removeUnit; unitName = "liters"}
            | ("quartsUs","minimsUs") -> {amount = quartsUsToMinimsUs(x*1m<quart_us>) |> removeUnit; unitName = "minimsUs"}
            | ("quartsUs","fluidDramsUs") -> {amount = quartsUsToFluidDramsUs(x*1m<quart_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("quartsUs","teaspoonsUs") -> {amount = quartsUsToTeaspoonsUs(x*1m<quart_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("quartsUs","fluidOuncesUs") -> {amount = quartsUsToFluidOuncesUs(x*1m<quart_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("quartsUs","tablespoonsUs") -> {amount = quartsUsToTablespoonsUs(x*1m<quart_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("quartsUs","shotsUs") -> {amount = quartsUsToShotsUs(x*1m<quart_us>) |> removeUnit; unitName = "shotsUs"}
            | ("quartsUs","gillsUs") -> {amount = quartsUsToGillsUs(x*1m<quart_us>) |> removeUnit; unitName = "gillsUs"}
            | ("quartsUs","cupsUs") -> {amount = quartsUsToCupsUs(x*1m<quart_us>) |> removeUnit; unitName = "cupsUs"}
            | ("quartsUs","pintsUs") -> {amount = quartsUsToPintsUs(x*1m<quart_us>) |> removeUnit; unitName = "pintsUs"}
            | ("quartsUs","pottlesUs") -> {amount = quartsUsToPottlesUs(x*1m<quart_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("quartsUs","gallonsUs") -> {amount = quartsUsToGallonsUs(x*1m<quart_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("quartsUs","barrelsUs") -> {amount = quartsUsToBarrelsUs(x*1m<quart_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("quartsUs","hogsheadsUs") -> {amount = quartsUsToHogsheadsUs(x*1m<quart_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("quartsUs","cubicInches") -> {amount = quartsUsToCubicInches(x*1m<quart_us>) |> removeUnit; unitName = "cubicInches"}
            | ("quartsUs","minimsUk") -> {amount = quartsUsToMinimsUk(x*1m<quart_us>) |> removeUnit; unitName = "minimsUk"}
            | ("quartsUs","fluidDrachmsUk") -> {amount = quartsUsToFluidDrachmsUk(x*1m<quart_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("quartsUs","fluidOuncesUk") -> {amount = quartsUsToFluidOuncesUk(x*1m<quart_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("quartsUs","gillsUk") -> {amount = quartsUsToGillsUk(x*1m<quart_us>) |> removeUnit; unitName = "gillsUk"}
            | ("quartsUs","pintsUk") -> {amount = quartsUsToPintsUk(x*1m<quart_us>) |> removeUnit; unitName = "pintsUk"}
            | ("quartsUs","quartsUk") -> {amount = quartsUsToQuartsUk(x*1m<quart_us>) |> removeUnit; unitName = "quartsUk"}
            | ("quartsUs","gallonsUk") -> {amount = quartsUsToGallonsUk(x*1m<quart_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("quartsUs","pecksUk") -> {amount = quartsUsToPecksUk(x*1m<quart_us>) |> removeUnit; unitName = "pecksUk"}
            | ("quartsUs","bushelsUk") -> {amount = quartsUsToBushelsUk(x*1m<quart_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("quartsUs","quartersUk") -> {amount = quartsUsToQuartersUk(x*1m<quart_us>) |> removeUnit; unitName = "quartersUk"}
            | ("pottlesUs","milliliters") -> {amount = pottlesUsToMilliliters(x*1m<pottle_us>) |> removeUnit; unitName = "milliliters"}
            | ("pottlesUs","liters") -> {amount = pottlesUsToLiters(x*1m<pottle_us>) |> removeUnit; unitName = "liters"}
            | ("pottlesUs","minimsUs") -> {amount = pottlesUsToMinimsUs(x*1m<pottle_us>) |> removeUnit; unitName = "minimsUs"}
            | ("pottlesUs","fluidDramsUs") -> {amount = pottlesUsToFluidDramsUs(x*1m<pottle_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("pottlesUs","teaspoonsUs") -> {amount = pottlesUsToTeaspoonsUs(x*1m<pottle_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("pottlesUs","fluidOuncesUs") -> {amount = pottlesUsToFluidOuncesUs(x*1m<pottle_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("pottlesUs","tablespoonsUs") -> {amount = pottlesUsToTablespoonsUs(x*1m<pottle_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("pottlesUs","shotsUs") -> {amount = pottlesUsToShotsUs(x*1m<pottle_us>) |> removeUnit; unitName = "shotsUs"}
            | ("pottlesUs","gillsUs") -> {amount = pottlesUsToGillsUs(x*1m<pottle_us>) |> removeUnit; unitName = "gillsUs"}
            | ("pottlesUs","cupsUs") -> {amount = pottlesUsToCupsUs(x*1m<pottle_us>) |> removeUnit; unitName = "cupsUs"}
            | ("pottlesUs","pintsUs") -> {amount = pottlesUsToPintsUs(x*1m<pottle_us>) |> removeUnit; unitName = "pintsUs"}
            | ("pottlesUs","quartsUs") -> {amount = pottlesUsToQuartsUs(x*1m<pottle_us>) |> removeUnit; unitName = "quartsUs"}
            | ("pottlesUs","gallonsUs") -> {amount = pottlesUsToGallonsUs(x*1m<pottle_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("pottlesUs","barrelsUs") -> {amount = pottlesUsToBarrelsUs(x*1m<pottle_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("pottlesUs","hogsheadsUs") -> {amount = pottlesUsToHogsheadsUs(x*1m<pottle_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("pottlesUs","cubicInches") -> {amount = pottlesUsToCubicInches(x*1m<pottle_us>) |> removeUnit; unitName = "cubicInches"}
            | ("pottlesUs","minimsUk") -> {amount = pottlesUsToMinimsUk(x*1m<pottle_us>) |> removeUnit; unitName = "minimsUk"}
            | ("pottlesUs","fluidDrachmsUk") -> {amount = pottlesUsToFluidDrachmsUk(x*1m<pottle_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("pottlesUs","fluidOuncesUk") -> {amount = pottlesUsToFluidOuncesUk(x*1m<pottle_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("pottlesUs","gillsUk") -> {amount = pottlesUsToGillsUk(x*1m<pottle_us>) |> removeUnit; unitName = "gillsUk"}
            | ("pottlesUs","pintsUk") -> {amount = pottlesUsToPintsUk(x*1m<pottle_us>) |> removeUnit; unitName = "pintsUk"}
            | ("pottlesUs","quartsUk") -> {amount = pottlesUsToQuartsUk(x*1m<pottle_us>) |> removeUnit; unitName = "quartsUk"}
            | ("pottlesUs","gallonsUk") -> {amount = pottlesUsToGallonsUk(x*1m<pottle_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("pottlesUs","pecksUk") -> {amount = pottlesUsToPecksUk(x*1m<pottle_us>) |> removeUnit; unitName = "pecksUk"}
            | ("pottlesUs","bushelsUk") -> {amount = pottlesUsToBushelsUk(x*1m<pottle_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("pottlesUs","quartersUk") -> {amount = pottlesUsToQuartersUk(x*1m<pottle_us>) |> removeUnit; unitName = "quartersUk"}
            | ("gallonsUs","milliliters") -> {amount = gallonsUsToMilliliters(x*1m<gallon_us>) |> removeUnit; unitName = "milliliters"}
            | ("gallonsUs","liters") -> {amount = gallonsUsToLiters(x*1m<gallon_us>) |> removeUnit; unitName = "liters"}
            | ("gallonsUs","minimsUs") -> {amount = gallonsUsToMinimsUs(x*1m<gallon_us>) |> removeUnit; unitName = "minimsUs"}
            | ("gallonsUs","fluidDramsUs") -> {amount = gallonsUsToFluidDramsUs(x*1m<gallon_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("gallonsUs","teaspoonsUs") -> {amount = gallonsUsToTeaspoonsUs(x*1m<gallon_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("gallonsUs","fluidOuncesUs") -> {amount = gallonsUsToFluidOuncesUs(x*1m<gallon_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("gallonsUs","tablespoonsUs") -> {amount = gallonsUsToTablespoonsUs(x*1m<gallon_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("gallonsUs","shotsUs") -> {amount = gallonsUsToShotsUs(x*1m<gallon_us>) |> removeUnit; unitName = "shotsUs"}
            | ("gallonsUs","gillsUs") -> {amount = gallonsUsToGillsUs(x*1m<gallon_us>) |> removeUnit; unitName = "gillsUs"}
            | ("gallonsUs","cupsUs") -> {amount = gallonsUsToCupsUs(x*1m<gallon_us>) |> removeUnit; unitName = "cupsUs"}
            | ("gallonsUs","pintsUs") -> {amount = gallonsUsToPintsUs(x*1m<gallon_us>) |> removeUnit; unitName = "pintsUs"}
            | ("gallonsUs","quartsUs") -> {amount = gallonsUsToQuartsUs(x*1m<gallon_us>) |> removeUnit; unitName = "quartsUs"}
            | ("gallonsUs","pottlesUs") -> {amount = gallonsUsToPottlesUs(x*1m<gallon_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("gallonsUs","barrelsUs") -> {amount = gallonsUsToBarrelsUs(x*1m<gallon_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("gallonsUs","hogsheadsUs") -> {amount = gallonsUsToHogsheadsUs(x*1m<gallon_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("gallonsUs","cubicInches") -> {amount = gallonsUsToCubicInches(x*1m<gallon_us>) |> removeUnit; unitName = "cubicInches"}
            | ("gallonsUs","minimsUk") -> {amount = gallonsUsToMinimsUk(x*1m<gallon_us>) |> removeUnit; unitName = "minimsUk"}
            | ("gallonsUs","fluidDrachmsUk") -> {amount = gallonsUsToFluidDrachmsUk(x*1m<gallon_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("gallonsUs","fluidOuncesUk") -> {amount = gallonsUsToFluidOuncesUk(x*1m<gallon_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("gallonsUs","gillsUk") -> {amount = gallonsUsToGillsUk(x*1m<gallon_us>) |> removeUnit; unitName = "gillsUk"}
            | ("gallonsUs","pintsUk") -> {amount = gallonsUsToPintsUk(x*1m<gallon_us>) |> removeUnit; unitName = "pintsUk"}
            | ("gallonsUs","quartsUk") -> {amount = gallonsUsToQuartsUk(x*1m<gallon_us>) |> removeUnit; unitName = "quartsUk"}
            | ("gallonsUs","gallonsUk") -> {amount = gallonsUsToGallonsUk(x*1m<gallon_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("gallonsUs","pecksUk") -> {amount = gallonsUsToPecksUk(x*1m<gallon_us>) |> removeUnit; unitName = "pecksUk"}
            | ("gallonsUs","bushelsUk") -> {amount = gallonsUsToBushelsUk(x*1m<gallon_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("gallonsUs","quartersUk") -> {amount = gallonsUsToQuartersUk(x*1m<gallon_us>) |> removeUnit; unitName = "quartersUk"}
            | ("barrelsUs","milliliters") -> {amount = barrelsUsToMilliliters(x*1m<barrel_us>) |> removeUnit; unitName = "milliliters"}
            | ("barrelsUs","liters") -> {amount = barrelsUsToLiters(x*1m<barrel_us>) |> removeUnit; unitName = "liters"}
            | ("barrelsUs","minimsUs") -> {amount = barrelsUsToMinimsUs(x*1m<barrel_us>) |> removeUnit; unitName = "minimsUs"}
            | ("barrelsUs","fluidDramsUs") -> {amount = barrelsUsToFluidDramsUs(x*1m<barrel_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("barrelsUs","teaspoonsUs") -> {amount = barrelsUsToTeaspoonsUs(x*1m<barrel_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("barrelsUs","fluidOuncesUs") -> {amount = barrelsUsToFluidOuncesUs(x*1m<barrel_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("barrelsUs","tablespoonsUs") -> {amount = barrelsUsToTablespoonsUs(x*1m<barrel_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("barrelsUs","shotsUs") -> {amount = barrelsUsToShotsUs(x*1m<barrel_us>) |> removeUnit; unitName = "shotsUs"}
            | ("barrelsUs","gillsUs") -> {amount = barrelsUsToGillsUs(x*1m<barrel_us>) |> removeUnit; unitName = "gillsUs"}
            | ("barrelsUs","cupsUs") -> {amount = barrelsUsToCupsUs(x*1m<barrel_us>) |> removeUnit; unitName = "cupsUs"}
            | ("barrelsUs","pintsUs") -> {amount = barrelsUsToPintsUs(x*1m<barrel_us>) |> removeUnit; unitName = "pintsUs"}
            | ("barrelsUs","quartsUs") -> {amount = barrelsUsToQuartsUs(x*1m<barrel_us>) |> removeUnit; unitName = "quartsUs"}
            | ("barrelsUs","pottlesUs") -> {amount = barrelsUsToPottlesUs(x*1m<barrel_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("barrelsUs","gallonsUs") -> {amount = barrelsUsToGallonsUs(x*1m<barrel_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("barrelsUs","hogsheadsUs") -> {amount = barrelsUsToHogsheadsUs(x*1m<barrel_us>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("barrelsUs","cubicInches") -> {amount = barrelsUsToCubicInches(x*1m<barrel_us>) |> removeUnit; unitName = "cubicInches"}
            | ("barrelsUs","minimsUk") -> {amount = barrelsUsToMinimsUk(x*1m<barrel_us>) |> removeUnit; unitName = "minimsUk"}
            | ("barrelsUs","fluidDrachmsUk") -> {amount = barrelsUsToFluidDrachmsUk(x*1m<barrel_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("barrelsUs","fluidOuncesUk") -> {amount = barrelsUsToFluidOuncesUk(x*1m<barrel_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("barrelsUs","gillsUk") -> {amount = barrelsUsToGillsUk(x*1m<barrel_us>) |> removeUnit; unitName = "gillsUk"}
            | ("barrelsUs","pintsUk") -> {amount = barrelsUsToPintsUk(x*1m<barrel_us>) |> removeUnit; unitName = "pintsUk"}
            | ("barrelsUs","quartsUk") -> {amount = barrelsUsToQuartsUk(x*1m<barrel_us>) |> removeUnit; unitName = "quartsUk"}
            | ("barrelsUs","gallonsUk") -> {amount = barrelsUsToGallonsUk(x*1m<barrel_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("barrelsUs","pecksUk") -> {amount = barrelsUsToPecksUk(x*1m<barrel_us>) |> removeUnit; unitName = "pecksUk"}
            | ("barrelsUs","bushelsUk") -> {amount = barrelsUsToBushelsUk(x*1m<barrel_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("barrelsUs","quartersUk") -> {amount = barrelsUsToQuartersUk(x*1m<barrel_us>) |> removeUnit; unitName = "quartersUk"}
            | ("hogsheadsUs","milliliters") -> {amount = hogsheadsUsToMilliliters(x*1m<hogshead_us>) |> removeUnit; unitName = "milliliters"}
            | ("hogsheadsUs","liters") -> {amount = hogsheadsUsToLiters(x*1m<hogshead_us>) |> removeUnit; unitName = "liters"}
            | ("hogsheadsUs","minimsUs") -> {amount = hogsheadsUsToMinimsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "minimsUs"}
            | ("hogsheadsUs","fluidDramsUs") -> {amount = hogsheadsUsToFluidDramsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("hogsheadsUs","teaspoonsUs") -> {amount = hogsheadsUsToTeaspoonsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("hogsheadsUs","fluidOuncesUs") -> {amount = hogsheadsUsToFluidOuncesUs(x*1m<hogshead_us>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("hogsheadsUs","tablespoonsUs") -> {amount = hogsheadsUsToTablespoonsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("hogsheadsUs","shotsUs") -> {amount = hogsheadsUsToShotsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "shotsUs"}
            | ("hogsheadsUs","gillsUs") -> {amount = hogsheadsUsToGillsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "gillsUs"}
            | ("hogsheadsUs","cupsUs") -> {amount = hogsheadsUsToCupsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "cupsUs"}
            | ("hogsheadsUs","pintsUs") -> {amount = hogsheadsUsToPintsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "pintsUs"}
            | ("hogsheadsUs","quartsUs") -> {amount = hogsheadsUsToQuartsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "quartsUs"}
            | ("hogsheadsUs","pottlesUs") -> {amount = hogsheadsUsToPottlesUs(x*1m<hogshead_us>) |> removeUnit; unitName = "pottlesUs"}
            | ("hogsheadsUs","gallonsUs") -> {amount = hogsheadsUsToGallonsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "gallonsUs"}
            | ("hogsheadsUs","barrelsUs") -> {amount = hogsheadsUsToBarrelsUs(x*1m<hogshead_us>) |> removeUnit; unitName = "barrelsUs"}
            | ("hogsheadsUs","cubicInches") -> {amount = hogsheadsUsToCubicInches(x*1m<hogshead_us>) |> removeUnit; unitName = "cubicInches"}
            | ("hogsheadsUs","minimsUk") -> {amount = hogsheadsUsToMinimsUk(x*1m<hogshead_us>) |> removeUnit; unitName = "minimsUk"}
            | ("hogsheadsUs","fluidDrachmsUk") -> {amount = hogsheadsUsToFluidDrachmsUk(x*1m<hogshead_us>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("hogsheadsUs","fluidOuncesUk") -> {amount = hogsheadsUsToFluidOuncesUk(x*1m<hogshead_us>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("hogsheadsUs","gillsUk") -> {amount = hogsheadsUsToGillsUk(x*1m<hogshead_us>) |> removeUnit; unitName = "gillsUk"}
            | ("hogsheadsUs","pintsUk") -> {amount = hogsheadsUsToPintsUk(x*1m<hogshead_us>) |> removeUnit; unitName = "pintsUk"}
            | ("hogsheadsUs","quartsUk") -> {amount = hogsheadsUsToQuartsUk(x*1m<hogshead_us>) |> removeUnit; unitName = "quartsUk"}
            | ("hogsheadsUs","gallonsUk") -> {amount = hogsheadsUsToGallonsUk(x*1m<hogshead_us>) |> removeUnit; unitName = "gallonsUk"}
            | ("hogsheadsUs","pecksUk") -> {amount = hogsheadsUsToPecksUk(x*1m<hogshead_us>) |> removeUnit; unitName = "pecksUk"}
            | ("hogsheadsUs","bushelsUk") -> {amount = hogsheadsUsToBushelsUk(x*1m<hogshead_us>) |> removeUnit; unitName = "bushelsUk"}
            | ("hogsheadsUs","quartersUk") -> {amount = hogsheadsUsToQuartersUk(x*1m<hogshead_us>) |> removeUnit; unitName = "quartersUk"}
            | ("cubicInches","milliliters") -> {amount = cubicInchesToMilliliters(x*1m<cubicInch>) |> removeUnit; unitName = "milliliters"}
            | ("cubicInches","liters") -> {amount = cubicInchesToLiters(x*1m<cubicInch>) |> removeUnit; unitName = "liters"}
            | ("cubicInches","minimsUs") -> {amount = cubicInchesToMinimsUs(x*1m<cubicInch>) |> removeUnit; unitName = "minimsUs"}
            | ("cubicInches","fluidDramsUs") -> {amount = cubicInchesToFluidDramsUs(x*1m<cubicInch>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("cubicInches","teaspoonsUs") -> {amount = cubicInchesToTeaspoonsUs(x*1m<cubicInch>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("cubicInches","fluidOuncesUs") -> {amount = cubicInchesToFluidOuncesUs(x*1m<cubicInch>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("cubicInches","tablespoonsUs") -> {amount = cubicInchesToTablespoonsUs(x*1m<cubicInch>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("cubicInches","shotsUs") -> {amount = cubicInchesToShotsUs(x*1m<cubicInch>) |> removeUnit; unitName = "shotsUs"}
            | ("cubicInches","gillsUs") -> {amount = cubicInchesToGillsUs(x*1m<cubicInch>) |> removeUnit; unitName = "gillsUs"}
            | ("cubicInches","cupsUs") -> {amount = cubicInchesToCupsUs(x*1m<cubicInch>) |> removeUnit; unitName = "cupsUs"}
            | ("cubicInches","pintsUs") -> {amount = cubicInchesToPintsUs(x*1m<cubicInch>) |> removeUnit; unitName = "pintsUs"}
            | ("cubicInches","quartsUs") -> {amount = cubicInchesToQuartsUs(x*1m<cubicInch>) |> removeUnit; unitName = "quartsUs"}
            | ("cubicInches","pottlesUs") -> {amount = cubicInchesToPottlesUs(x*1m<cubicInch>) |> removeUnit; unitName = "pottlesUs"}
            | ("cubicInches","gallonsUs") -> {amount = cubicInchesToGallonsUs(x*1m<cubicInch>) |> removeUnit; unitName = "gallonsUs"}
            | ("cubicInches","barrelsUs") -> {amount = cubicInchesToBarrelsUs(x*1m<cubicInch>) |> removeUnit; unitName = "barrelsUs"}
            | ("cubicInches","hogsheadsUs") -> {amount = cubicInchesToHogsheadsUs(x*1m<cubicInch>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("cubicInches","minimsUk") -> {amount = cubicInchesToMinimsUk(x*1m<cubicInch>) |> removeUnit; unitName = "minimsUk"}
            | ("cubicInches","fluidDrachmsUk") -> {amount = cubicInchesToFluidDrachmsUk(x*1m<cubicInch>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("cubicInches","fluidOuncesUk") -> {amount = cubicInchesToFluidOuncesUk(x*1m<cubicInch>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("cubicInches","gillsUk") -> {amount = cubicInchesToGillsUk(x*1m<cubicInch>) |> removeUnit; unitName = "gillsUk"}
            | ("cubicInches","pintsUk") -> {amount = cubicInchesToPintsUk(x*1m<cubicInch>) |> removeUnit; unitName = "pintsUk"}
            | ("cubicInches","quartsUk") -> {amount = cubicInchesToQuartsUk(x*1m<cubicInch>) |> removeUnit; unitName = "quartsUk"}
            | ("cubicInches","gallonsUk") -> {amount = cubicInchesToGallonsUk(x*1m<cubicInch>) |> removeUnit; unitName = "gallonsUk"}
            | ("cubicInches","pecksUk") -> {amount = cubicInchesToPecksUk(x*1m<cubicInch>) |> removeUnit; unitName = "pecksUk"}
            | ("cubicInches","bushelsUk") -> {amount = cubicInchesToBushelsUk(x*1m<cubicInch>) |> removeUnit; unitName = "bushelsUk"}
            | ("cubicInches","quartersUk") -> {amount = cubicInchesToQuartersUk(x*1m<cubicInch>) |> removeUnit; unitName = "quartersUk"}
            | ("minimsUk","milliliters") -> {amount = minimsUkToMilliliters(x*1m<minim_uk>) |> removeUnit; unitName = "milliliters"}
            | ("minimsUk","liters") -> {amount = minimsUkToLiters(x*1m<minim_uk>) |> removeUnit; unitName = "liters"}
            | ("minimsUk","minimsUs") -> {amount = minimsUkToMinimsUs(x*1m<minim_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("minimsUk","fluidDramsUs") -> {amount = minimsUkToFluidDramsUs(x*1m<minim_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("minimsUk","teaspoonsUs") -> {amount = minimsUkToTeaspoonsUs(x*1m<minim_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("minimsUk","fluidOuncesUs") -> {amount = minimsUkToFluidOuncesUs(x*1m<minim_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("minimsUk","tablespoonsUs") -> {amount = minimsUkToTablespoonsUs(x*1m<minim_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("minimsUk","shotsUs") -> {amount = minimsUkToShotsUs(x*1m<minim_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("minimsUk","gillsUs") -> {amount = minimsUkToGillsUs(x*1m<minim_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("minimsUk","cupsUs") -> {amount = minimsUkToCupsUs(x*1m<minim_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("minimsUk","pintsUs") -> {amount = minimsUkToPintsUs(x*1m<minim_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("minimsUk","quartsUs") -> {amount = minimsUkToQuartsUs(x*1m<minim_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("minimsUk","pottlesUs") -> {amount = minimsUkToPottlesUs(x*1m<minim_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("minimsUk","gallonsUs") -> {amount = minimsUkToGallonsUs(x*1m<minim_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("minimsUk","barrelsUs") -> {amount = minimsUkToBarrelsUs(x*1m<minim_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("minimsUk","hogsheadsUs") -> {amount = minimsUkToHogsheadsUs(x*1m<minim_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("minimsUk","cubicInches") -> {amount = minimsUkToCubicInches(x*1m<minim_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("minimsUk","fluidDrachmsUk") -> {amount = minimsUkToFluidDrachmsUk(x*1m<minim_uk>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("minimsUk","fluidOuncesUk") -> {amount = minimsUkToFluidOuncesUk(x*1m<minim_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("minimsUk","gillsUk") -> {amount = minimsUkToGillsUk(x*1m<minim_uk>) |> removeUnit; unitName = "gillsUk"}
            | ("minimsUk","pintsUk") -> {amount = minimsUkToPintsUk(x*1m<minim_uk>) |> removeUnit; unitName = "pintsUk"}
            | ("minimsUk","quartsUk") -> {amount = minimsUkToQuartsUk(x*1m<minim_uk>) |> removeUnit; unitName = "quartsUk"}
            | ("minimsUk","gallonsUk") -> {amount = minimsUkToGallonsUk(x*1m<minim_uk>) |> removeUnit; unitName = "gallonsUk"}
            | ("minimsUk","pecksUk") -> {amount = minimsUkToPecksUk(x*1m<minim_uk>) |> removeUnit; unitName = "pecksUk"}
            | ("minimsUk","bushelsUk") -> {amount = minimsUkToBushelsUk(x*1m<minim_uk>) |> removeUnit; unitName = "bushelsUk"}
            | ("minimsUk","quartersUk") -> {amount = minimsUkToQuartersUk(x*1m<minim_uk>) |> removeUnit; unitName = "quartersUk"}
            | ("fluidDrachmsUk","milliliters") -> {amount = fluidDrachmsUkToMilliliters(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "milliliters"}
            | ("fluidDrachmsUk","liters") -> {amount = fluidDrachmsUkToLiters(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "liters"}
            | ("fluidDrachmsUk","minimsUs") -> {amount = fluidDrachmsUkToMinimsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("fluidDrachmsUk","fluidDramsUs") -> {amount = fluidDrachmsUkToFluidDramsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("fluidDrachmsUk","teaspoonsUs") -> {amount = fluidDrachmsUkToTeaspoonsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("fluidDrachmsUk","fluidOuncesUs") -> {amount = fluidDrachmsUkToFluidOuncesUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("fluidDrachmsUk","tablespoonsUs") -> {amount = fluidDrachmsUkToTablespoonsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("fluidDrachmsUk","shotsUs") -> {amount = fluidDrachmsUkToShotsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("fluidDrachmsUk","gillsUs") -> {amount = fluidDrachmsUkToGillsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("fluidDrachmsUk","cupsUs") -> {amount = fluidDrachmsUkToCupsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("fluidDrachmsUk","pintsUs") -> {amount = fluidDrachmsUkToPintsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("fluidDrachmsUk","quartsUs") -> {amount = fluidDrachmsUkToQuartsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("fluidDrachmsUk","pottlesUs") -> {amount = fluidDrachmsUkToPottlesUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("fluidDrachmsUk","gallonsUs") -> {amount = fluidDrachmsUkToGallonsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("fluidDrachmsUk","barrelsUs") -> {amount = fluidDrachmsUkToBarrelsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("fluidDrachmsUk","hogsheadsUs") -> {amount = fluidDrachmsUkToHogsheadsUs(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("fluidDrachmsUk","cubicInches") -> {amount = fluidDrachmsUkToCubicInches(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("fluidDrachmsUk","minimsUk") -> {amount = fluidDrachmsUkToMinimsUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "minimsUk"}
            | ("fluidDrachmsUk","fluidOuncesUk") -> {amount = fluidDrachmsUkToFluidOuncesUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("fluidDrachmsUk","gillsUk") -> {amount = fluidDrachmsUkToGillsUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "gillsUk"}
            | ("fluidDrachmsUk","pintsUk") -> {amount = fluidDrachmsUkToPintsUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "pintsUk"}
            | ("fluidDrachmsUk","quartsUk") -> {amount = fluidDrachmsUkToQuartsUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "quartsUk"}
            | ("fluidDrachmsUk","gallonsUk") -> {amount = fluidDrachmsUkToGallonsUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "gallonsUk"}
            | ("fluidDrachmsUk","pecksUk") -> {amount = fluidDrachmsUkToPecksUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "pecksUk"}
            | ("fluidDrachmsUk","bushelsUk") -> {amount = fluidDrachmsUkToBushelsUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "bushelsUk"}
            | ("fluidDrachmsUk","quartersUk") -> {amount = fluidDrachmsUkToQuartersUk(x*1m<fluidDrachm_uk>) |> removeUnit; unitName = "quartersUk"}
            | ("gillsUk","milliliters") -> {amount = gillsUkToMilliliters(x*1m<gill_uk>) |> removeUnit; unitName = "milliliters"}
            | ("gillsUk","liters") -> {amount = gillsUkToLiters(x*1m<gill_uk>) |> removeUnit; unitName = "liters"}
            | ("gillsUk","minimsUs") -> {amount = gillsUkToMinimsUs(x*1m<gill_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("gillsUk","fluidDramsUs") -> {amount = gillsUkToFluidDramsUs(x*1m<gill_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("gillsUk","teaspoonsUs") -> {amount = gillsUkToTeaspoonsUs(x*1m<gill_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("gillsUk","fluidOuncesUs") -> {amount = gillsUkToFluidOuncesUs(x*1m<gill_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("gillsUk","tablespoonsUs") -> {amount = gillsUkToTablespoonsUs(x*1m<gill_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("gillsUk","shotsUs") -> {amount = gillsUkToShotsUs(x*1m<gill_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("gillsUk","gillsUs") -> {amount = gillsUkToGillsUs(x*1m<gill_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("gillsUk","cupsUs") -> {amount = gillsUkToCupsUs(x*1m<gill_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("gillsUk","pintsUs") -> {amount = gillsUkToPintsUs(x*1m<gill_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("gillsUk","quartsUs") -> {amount = gillsUkToQuartsUs(x*1m<gill_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("gillsUk","pottlesUs") -> {amount = gillsUkToPottlesUs(x*1m<gill_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("gillsUk","gallonsUs") -> {amount = gillsUkToGallonsUs(x*1m<gill_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("gillsUk","barrelsUs") -> {amount = gillsUkToBarrelsUs(x*1m<gill_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("gillsUk","hogsheadsUs") -> {amount = gillsUkToHogsheadsUs(x*1m<gill_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("gillsUk","cubicInches") -> {amount = gillsUkToCubicInches(x*1m<gill_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("gillsUk","minimsUk") -> {amount = gillsUkToMinimsUk(x*1m<gill_uk>) |> removeUnit; unitName = "minimsUk"}
            | ("gillsUk","fluidOuncesUk") -> {amount = gillsUkToFluidOuncesUk(x*1m<gill_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("gillsUk","fluidDrachmsUk") -> {amount = gillsUkToFluidDrachmsUk(x*1m<gill_uk>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("gillsUk","pintsUk") -> {amount = gillsUkToPintsUk(x*1m<gill_uk>) |> removeUnit; unitName = "pintsUk"}
            | ("gillsUk","quartsUk") -> {amount = gillsUkToQuartsUk(x*1m<gill_uk>) |> removeUnit; unitName = "quartsUk"}
            | ("gillsUk","gallonsUk") -> {amount = gillsUkToGallonsUk(x*1m<gill_uk>) |> removeUnit; unitName = "gallonsUk"}
            | ("gillsUk","pecksUk") -> {amount = gillsUkToPecksUk(x*1m<gill_uk>) |> removeUnit; unitName = "pecksUk"}
            | ("gillsUk","bushelsUk") -> {amount = gillsUkToBushelsUk(x*1m<gill_uk>) |> removeUnit; unitName = "bushelsUk"}
            | ("gillsUk","quartersUk") -> {amount = gillsUkToQuartersUk(x*1m<gill_uk>) |> removeUnit; unitName = "quartersUk"}
            | ("pintsUk","milliliters") -> {amount = pintsUkToMilliliters(x*1m<pint_uk>) |> removeUnit; unitName = "milliliters"}
            | ("pintsUk","liters") -> {amount = pintsUkToLiters(x*1m<pint_uk>) |> removeUnit; unitName = "liters"}
            | ("pintsUk","minimsUs") -> {amount = pintsUkToMinimsUs(x*1m<pint_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("pintsUk","fluidDramsUs") -> {amount = pintsUkToFluidDramsUs(x*1m<pint_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("pintsUk","teaspoonsUs") -> {amount = pintsUkToTeaspoonsUs(x*1m<pint_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("pintsUk","fluidOuncesUs") -> {amount = pintsUkToFluidOuncesUs(x*1m<pint_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("pintsUk","tablespoonsUs") -> {amount = pintsUkToTablespoonsUs(x*1m<pint_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("pintsUk","shotsUs") -> {amount = pintsUkToShotsUs(x*1m<pint_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("pintsUk","gillsUs") -> {amount = pintsUkToGillsUs(x*1m<pint_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("pintsUk","cupsUs") -> {amount = pintsUkToCupsUs(x*1m<pint_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("pintsUk","pintsUs") -> {amount = pintsUkToPintsUs(x*1m<pint_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("pintsUk","quartsUs") -> {amount = pintsUkToQuartsUs(x*1m<pint_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("pintsUk","pottlesUs") -> {amount = pintsUkToPottlesUs(x*1m<pint_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("pintsUk","gallonsUs") -> {amount = pintsUkToGallonsUs(x*1m<pint_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("pintsUk","barrelsUs") -> {amount = pintsUkToBarrelsUs(x*1m<pint_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("pintsUk","hogsheadsUs") -> {amount = pintsUkToHogsheadsUs(x*1m<pint_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("pintsUk","cubicInches") -> {amount = pintsUkToCubicInches(x*1m<pint_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("pintsUk","minimsUk") -> {amount = pintsUkToMinimsUk(x*1m<pint_uk>) |> removeUnit; unitName = "minimsUk"}
            | ("pintsUk","fluidOuncesUk") -> {amount = pintsUkToFluidOuncesUk(x*1m<pint_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("pintsUk","fluidDrachmsUk") -> {amount = pintsUkToFluidDrachmsUk(x*1m<pint_uk>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("pintsUk","gillsUk") -> {amount = pintsUkToGillsUk(x*1m<pint_uk>) |> removeUnit; unitName = "gillsUk"}
            | ("pintsUk","quartsUk") -> {amount = pintsUkToQuartsUk(x*1m<pint_uk>) |> removeUnit; unitName = "quartsUk"}
            | ("pintsUk","gallonsUk") -> {amount = pintsUkToGallonsUk(x*1m<pint_uk>) |> removeUnit; unitName = "gallonsUk"}
            | ("pintsUk","pecksUk") -> {amount = pintsUkToPecksUk(x*1m<pint_uk>) |> removeUnit; unitName = "pecksUk"}
            | ("pintsUk","bushelsUk") -> {amount = pintsUkToBushelsUk(x*1m<pint_uk>) |> removeUnit; unitName = "bushelsUk"}
            | ("pintsUk","quartersUk") -> {amount = pintsUkToQuartersUk(x*1m<pint_uk>) |> removeUnit; unitName = "quartersUk"}
            | ("quartsUk","milliliters") -> {amount = quartsUkToMilliliters(x*1m<quart_uk>) |> removeUnit; unitName = "milliliters"}
            | ("quartsUk","liters") -> {amount = quartsUkToLiters(x*1m<quart_uk>) |> removeUnit; unitName = "liters"}
            | ("quartsUk","minimsUs") -> {amount = quartsUkToMinimsUs(x*1m<quart_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("quartsUk","fluidDramsUs") -> {amount = quartsUkToFluidDramsUs(x*1m<quart_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("quartsUk","teaspoonsUs") -> {amount = quartsUkToTeaspoonsUs(x*1m<quart_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("quartsUk","fluidOuncesUs") -> {amount = quartsUkToFluidOuncesUs(x*1m<quart_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("quartsUk","tablespoonsUs") -> {amount = quartsUkToTablespoonsUs(x*1m<quart_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("quartsUk","shotsUs") -> {amount = quartsUkToShotsUs(x*1m<quart_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("quartsUk","gillsUs") -> {amount = quartsUkToGillsUs(x*1m<quart_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("quartsUk","cupsUs") -> {amount = quartsUkToCupsUs(x*1m<quart_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("quartsUk","pintsUs") -> {amount = quartsUkToPintsUs(x*1m<quart_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("quartsUk","quartsUs") -> {amount = quartsUkToQuartsUs(x*1m<quart_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("quartsUk","pottlesUs") -> {amount = quartsUkToPottlesUs(x*1m<quart_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("quartsUk","gallonsUs") -> {amount = quartsUkToGallonsUs(x*1m<quart_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("quartsUk","barrelsUs") -> {amount = quartsUkToBarrelsUs(x*1m<quart_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("quartsUk","hogsheadsUs") -> {amount = quartsUkToHogsheadsUs(x*1m<quart_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("quartsUk","cubicInches") -> {amount = quartsUkToCubicInches(x*1m<quart_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("quartsUk","minimsUk") -> {amount = quartsUkToMinimsUk(x*1m<quart_uk>) |> removeUnit; unitName = "minimsUk"}
            | ("quartsUk","fluidOuncesUk") -> {amount = quartsUkToFluidOuncesUk(x*1m<quart_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("quartsUk","fluidDrachmsUk") -> {amount = quartsUkToFluidDrachmsUk(x*1m<quart_uk>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("quartsUk","gillsUk") -> {amount = quartsUkToGillsUk(x*1m<quart_uk>) |> removeUnit; unitName = "gillsUk"}
            | ("quartsUk","pintsUk") -> {amount = quartsUkToPintsUk(x*1m<quart_uk>) |> removeUnit; unitName = "pintsUk"}
            | ("quartsUk","gallonsUk") -> {amount = quartsUkToGallonsUk(x*1m<quart_uk>) |> removeUnit; unitName = "gallonsUk"}
            | ("quartsUk","pecksUk") -> {amount = quartsUkToPecksUk(x*1m<quart_uk>) |> removeUnit; unitName = "pecksUk"}
            | ("quartsUk","bushelsUk") -> {amount = quartsUkToBushelsUk(x*1m<quart_uk>) |> removeUnit; unitName = "bushelsUk"}
            | ("quartsUk","quartersUk") -> {amount = quartsUkToQuartersUk(x*1m<quart_uk>) |> removeUnit; unitName = "quartersUk"}
            | ("gallonsUk","milliliters") -> {amount = gallonsUkToMilliliters(x*1m<gallon_uk>) |> removeUnit; unitName = "milliliters"}
            | ("gallonsUk","liters") -> {amount = gallonsUkToLiters(x*1m<gallon_uk>) |> removeUnit; unitName = "liters"}
            | ("gallonsUk","minimsUs") -> {amount = gallonsUkToMinimsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("gallonsUk","fluidDramsUs") -> {amount = gallonsUkToFluidDramsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("gallonsUk","teaspoonsUs") -> {amount = gallonsUkToTeaspoonsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("gallonsUk","fluidOuncesUs") -> {amount = gallonsUkToFluidOuncesUs(x*1m<gallon_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("gallonsUk","tablespoonsUs") -> {amount = gallonsUkToTablespoonsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("gallonsUk","shotsUs") -> {amount = gallonsUkToShotsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("gallonsUk","gillsUs") -> {amount = gallonsUkToGillsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("gallonsUk","cupsUs") -> {amount = gallonsUkToCupsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("gallonsUk","pintsUs") -> {amount = gallonsUkToPintsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("gallonsUk","quartsUs") -> {amount = gallonsUkToQuartsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("gallonsUk","pottlesUs") -> {amount = gallonsUkToPottlesUs(x*1m<gallon_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("gallonsUk","gallonsUs") -> {amount = gallonsUkToGallonsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("gallonsUk","barrelsUs") -> {amount = gallonsUkToBarrelsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("gallonsUk","hogsheadsUs") -> {amount = gallonsUkToHogsheadsUs(x*1m<gallon_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("gallonsUk","cubicInches") -> {amount = gallonsUkToCubicInches(x*1m<gallon_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("gallonsUk","minimsUk") -> {amount = gallonsUkToMinimsUk(x*1m<gallon_uk>) |> removeUnit; unitName = "minimsUk"}
            | ("gallonsUk","fluidOuncesUk") -> {amount = gallonsUkToFluidOuncesUk(x*1m<gallon_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("gallonsUk","fluidDrachmsUk") -> {amount = gallonsUkToFluidDrachmsUk(x*1m<gallon_uk>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("gallonsUk","gillsUk") -> {amount = gallonsUkToGillsUk(x*1m<gallon_uk>) |> removeUnit; unitName = "gillsUk"}
            | ("gallonsUk","pintsUk") -> {amount = gallonsUkToPintsUk(x*1m<gallon_uk>) |> removeUnit; unitName = "pintsUk"}
            | ("gallonsUk","quartsUk") -> {amount = gallonsUkToQuartsUk(x*1m<gallon_uk>) |> removeUnit; unitName = "quartsUk"}
            | ("gallonsUk","pecksUk") -> {amount = gallonsUkToPecksUk(x*1m<gallon_uk>) |> removeUnit; unitName = "pecksUk"}
            | ("gallonsUk","bushelsUk") -> {amount = gallonsUkToBushelsUk(x*1m<gallon_uk>) |> removeUnit; unitName = "bushelsUk"}
            | ("gallonsUk","quartersUk") -> {amount = gallonsUkToQuartersUk(x*1m<gallon_uk>) |> removeUnit; unitName = "quartersUk"}
            | ("pecksUk","milliliters") -> {amount = pecksUkToMilliliters(x*1m<peck_uk>) |> removeUnit; unitName = "milliliters"}
            | ("pecksUk","liters") -> {amount = pecksUkToLiters(x*1m<peck_uk>) |> removeUnit; unitName = "liters"}
            | ("pecksUk","minimsUs") -> {amount = pecksUkToMinimsUs(x*1m<peck_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("pecksUk","fluidDramsUs") -> {amount = pecksUkToFluidDramsUs(x*1m<peck_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("pecksUk","teaspoonsUs") -> {amount = pecksUkToTeaspoonsUs(x*1m<peck_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("pecksUk","fluidOuncesUs") -> {amount = pecksUkToFluidOuncesUs(x*1m<peck_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("pecksUk","tablespoonsUs") -> {amount = pecksUkToTablespoonsUs(x*1m<peck_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("pecksUk","shotsUs") -> {amount = pecksUkToShotsUs(x*1m<peck_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("pecksUk","gillsUs") -> {amount = pecksUkToGillsUs(x*1m<peck_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("pecksUk","cupsUs") -> {amount = pecksUkToCupsUs(x*1m<peck_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("pecksUk","pintsUs") -> {amount = pecksUkToPintsUs(x*1m<peck_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("pecksUk","quartsUs") -> {amount = pecksUkToQuartsUs(x*1m<peck_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("pecksUk","pottlesUs") -> {amount = pecksUkToPottlesUs(x*1m<peck_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("pecksUk","gallonsUs") -> {amount = pecksUkToGallonsUs(x*1m<peck_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("pecksUk","barrelsUs") -> {amount = pecksUkToBarrelsUs(x*1m<peck_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("pecksUk","hogsheadsUs") -> {amount = pecksUkToHogsheadsUs(x*1m<peck_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("pecksUk","cubicInches") -> {amount = pecksUkToCubicInches(x*1m<peck_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("pecksUk","minimsUk") -> {amount = pecksUkToMinimsUk(x*1m<peck_uk>) |> removeUnit; unitName = "minimsUk"}
            | ("pecksUk","fluidOuncesUk") -> {amount = pecksUkToFluidOuncesUk(x*1m<peck_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("pecksUk","fluidDrachmsUk") -> {amount = pecksUkToFluidDrachmsUk(x*1m<peck_uk>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("pecksUk","gillsUk") -> {amount = pecksUkToGillsUk(x*1m<peck_uk>) |> removeUnit; unitName = "gillsUk"}
            | ("pecksUk","pintsUk") -> {amount = pecksUkToPintsUk(x*1m<peck_uk>) |> removeUnit; unitName = "pintsUk"}
            | ("pecksUk","quartsUk") -> {amount = pecksUkToQuartsUk(x*1m<peck_uk>) |> removeUnit; unitName = "quartsUk"}
            | ("pecksUk","gallonsUk") -> {amount = pecksUkToGallonsUk(x*1m<peck_uk>) |> removeUnit; unitName = "gallonsUk"}
            | ("pecksUk","bushelsUk") -> {amount = pecksUkToBushelsUk(x*1m<peck_uk>) |> removeUnit; unitName = "bushelsUk"}
            | ("pecksUk","quartersUk") -> {amount = pecksUkToQuartersUk(x*1m<peck_uk>) |> removeUnit; unitName = "quartersUk"}            
            | ("bushelsUk","milliliters") -> {amount = bushelsUkToMilliliters(x*1m<bushel_uk>) |> removeUnit; unitName = "milliliters"}
            | ("bushelsUk","liters") -> {amount = bushelsUkToLiters(x*1m<bushel_uk>) |> removeUnit; unitName = "liters"}
            | ("bushelsUk","minimsUs") -> {amount = bushelsUkToMinimsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("bushelsUk","fluidDramsUs") -> {amount = bushelsUkToFluidDramsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("bushelsUk","teaspoonsUs") -> {amount = bushelsUkToTeaspoonsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("bushelsUk","fluidOuncesUs") -> {amount = bushelsUkToFluidOuncesUs(x*1m<bushel_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("bushelsUk","tablespoonsUs") -> {amount = bushelsUkToTablespoonsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("bushelsUk","shotsUs") -> {amount = bushelsUkToShotsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("bushelsUk","gillsUs") -> {amount = bushelsUkToGillsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("bushelsUk","cupsUs") -> {amount = bushelsUkToCupsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("bushelsUk","pintsUs") -> {amount = bushelsUkToPintsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("bushelsUk","quartsUs") -> {amount = bushelsUkToQuartsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("bushelsUk","pottlesUs") -> {amount = bushelsUkToPottlesUs(x*1m<bushel_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("bushelsUk","gallonsUs") -> {amount = bushelsUkToGallonsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("bushelsUk","barrelsUs") -> {amount = bushelsUkToBarrelsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("bushelsUk","hogsheadsUs") -> {amount = bushelsUkToHogsheadsUs(x*1m<bushel_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("bushelsUk","cubicInches") -> {amount = bushelsUkToCubicInches(x*1m<bushel_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("bushelsUk","minimsUk") -> {amount = bushelsUkToMinimsUk(x*1m<bushel_uk>) |> removeUnit; unitName = "minimsUk"}
            | ("bushelsUk","fluidOuncesUk") -> {amount = bushelsUkToFluidOuncesUk(x*1m<bushel_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("bushelsUk","fluidDrachmsUk") -> {amount = bushelsUkToFluidDrachmsUk(x*1m<bushel_uk>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("bushelsUk","gillsUk") -> {amount = bushelsUkToGillsUk(x*1m<bushel_uk>) |> removeUnit; unitName = "gillsUk"}
            | ("bushelsUk","pintsUk") -> {amount = bushelsUkToPintsUk(x*1m<bushel_uk>) |> removeUnit; unitName = "pintsUk"}
            | ("bushelsUk","quartsUk") -> {amount = bushelsUkToQuartsUk(x*1m<bushel_uk>) |> removeUnit; unitName = "quartsUk"}
            | ("bushelsUk","gallonsUk") -> {amount = bushelsUkToGallonsUk(x*1m<bushel_uk>) |> removeUnit; unitName = "gallonsUk"}
            | ("bushelsUk","pecksUk") -> {amount = bushelsUkToPecksUk(x*1m<bushel_uk>) |> removeUnit; unitName = "pecksUk"}
            | ("bushelsUk","quartersUk") -> {amount = bushelsUkToQuartersUk(x*1m<bushel_uk>) |> removeUnit; unitName = "quartersUk"}
            | ("quartersUk","milliliters") -> {amount = quartersUkToMilliliters(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "milliliters"}
            | ("quartersUk","liters") -> {amount = quartersUkToLiters(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "liters"}
            | ("quartersUk","minimsUs") -> {amount = quartersUkToMinimsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "minimsUs"}
            | ("quartersUk","fluidDramsUs") -> {amount = quartersUkToFluidDramsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "fluidDramsUs"}
            | ("quartersUk","teaspoonsUs") -> {amount = quartersUkToTeaspoonsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "teaspoonsUs"}
            | ("quartersUk","fluidOuncesUs") -> {amount = quartersUkToFluidOuncesUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "fluidOuncesUs"}
            | ("quartersUk","tablespoonsUs") -> {amount = quartersUkToTablespoonsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "tablespoonsUs"}
            | ("quartersUk","shotsUs") -> {amount = quartersUkToShotsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "shotsUs"}
            | ("quartersUk","gillsUs") -> {amount = quartersUkToGillsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "gillsUs"}
            | ("quartersUk","cupsUs") -> {amount = quartersUkToCupsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "cupsUs"}
            | ("quartersUk","pintsUs") -> {amount = quartersUkToPintsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "pintsUs"}
            | ("quartersUk","quartsUs") -> {amount = quartersUkToQuartsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "quartsUs"}
            | ("quartersUk","pottlesUs") -> {amount = quartersUkToPottlesUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "pottlesUs"}
            | ("quartersUk","gallonsUs") -> {amount = quartersUkToGallonsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "gallonsUs"}
            | ("quartersUk","barrelsUs") -> {amount = quartersUkToBarrelsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "barrelsUs"}
            | ("quartersUk","hogsheadsUs") -> {amount = quartersUkToHogsheadsUs(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "hogsheadsUs"}
            | ("quartersUk","cubicInches") -> {amount = quartersUkToCubicInches(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "cubicInches"}
            | ("quartersUk","minimsUk") -> {amount = quartersUkToMinimsUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "minimsUk"}
            | ("quartersUk","fluidOuncesUk") -> {amount = quartersUkToFluidOuncesUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "fluidOuncesUk"}
            | ("quartersUk","fluidDrachmsUk") -> {amount = quartersUkToFluidDrachmsUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "fluidDrachmsUk"}
            | ("quartersUk","gillsUk") -> {amount = quartersUkToGillsUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "gillsUk"}
            | ("quartersUk","pintsUk") -> {amount = quartersUkToPintsUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "pintsUk"}
            | ("quartersUk","quartsUk") -> {amount = quartersUkToQuartsUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "quartsUk"}
            | ("quartersUk","gallonsUk") -> {amount = quartersUkToGallonsUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "gallonsUk"}
            | ("quartersUk","pecksUk") -> {amount = quartersUkToPecksUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "pecksUk"}
            | ("quartersUk","bushelsUk") -> {amount = quartersUkToBushelsUk(x*1m<quarter_vl_uk>) |> removeUnit; unitName = "bushelsUk"}
            | _ -> {amount = 0m; unitName = "conversionNotImplemented"}

        let convertVolume ((x:decimal),fromUnit,toUnit) =
            match List.contains fromUnit UnitsList.volumeList && List.contains toUnit UnitsList.volumeList with
            | true -> convertVolumeVerified(x,fromUnit,toUnit)
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