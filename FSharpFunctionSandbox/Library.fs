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
        let hogsheadsToBarrels (hogs: decimal<hogshead>) = hogs * barrelsPerHogshead

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
        let pecksToGallonsUk (pk: decimal<peck>) = pk * gallonsPerPeckUk
        let gallonsUkToPecks (gal: decimal<gallon_uk>) = gal / gallonsPerPeckUk
        let pecksToBushels (gal: decimal<peck>) = gal / pecksPerBushelUk
        let bushelsToPecks (bh: decimal<bushel>) = bh * pecksPerBushelUk
        let bushelsToQuarters (bh: decimal<bushel>) = bh / bushelsPerQuarterUk
        let quartersToBushels (qtr: decimal<quarter_vl>) = qtr * bushelsPerQuarterUk
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

        let millilitersToGallonsUs x = millilitersToLiters x |> litersToGallonsUs
        let millilitersToPottles x = millilitersToGallonsUs x |> gallonsUsToPottles
        let millilitersToQuartsUs x = millilitersToPottles x |> pottlesToQuartsUs
        let millilitersToPintsUs x = millilitersToQuartsUs x |> quartsUsToPintsUs
        let millilitersToCups x = millilitersToPintsUs x |> pintsUsToCups
        let millilitersToGillsUs x = millilitersToCups x |> cupsToGillsUs
        let millilitersToFluidOuncesUs x = millilitersToGillsUs x |> gillsUsToFluidOuncesUs
        let millilitersToTablespoons x = millilitersToFluidOuncesUs x |> fluidOuncesUsToTablespoons
        let millilitersToShots x = millilitersToTablespoons x |> tablespoonsToShots
        let millilitersToTeaspoons x = millilitersToTablespoons x |> tablespoonsToTeaspoons
        let millilitersToMinimsUs x = millilitersToTeaspoons x |> teaspoonsToMinimsUs
        let millilitersToFluidDramsUs x = millilitersToMinimsUs x |> minimsUsToFluidDramsUs
        let millilitersToBarrels x = millilitersToGallonsUs x |> gallonsUsToBarrels
        let millilitersToHogsheads x = millilitersToBarrels x |> barrelsToHogsheads
        let millilitersToCubicInches x = millilitersToGallonsUs x |> gallonsUsToCubicInches
        let millilitersToGallonsUk x = millilitersToLiters x |> litersToGallonsUk
        let millilitersToPecks x = millilitersToGallonsUk x |> gallonsUkToPecks
        let millilitersToQuartsUk x = millilitersToGallonsUk x |> gallonsUkToQuartsUk
        let millilitersToPintsUk x = millilitersToQuartsUk x |> quartsUkToPintsUk
        let millilitersToGillsUk x = millilitersToPintsUk x |> pintsUkToGillsUk
        let millilitersToBushels x = millilitersToPecks x |> pecksToBushels
        let millilitersToQuarters x = millilitersToBushels x |> bushelsToQuarters 
        let millilitersToFluidOuncesUk x = millilitersToGillsUk x |> gillsUkToFluidOuncesUk
        let millilitersToFluidDrachmsUk x = millilitersToFluidOuncesUk x |> fluidOuncesUkToFluidDrachmsUk
        let millilitersToMinimsUk x = millilitersToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUk
        
        let litersToBarrels x = litersToGallonsUs x |> gallonsUsToBarrels
        let litersToCubicInches x = litersToGallonsUs x |> gallonsUsToCubicInches
        let litersToPottles x = litersToGallonsUs x |> gallonsUsToPottles
        let litersToQuartsUs x = litersToPottles x |> pottlesToQuartsUs
        let litersToPintsUs x = litersToQuartsUs x |> quartsUsToPintsUs
        let litersToCups x = litersToPintsUs x |> pintsUsToCups
        let litersToGillsUs x = litersToCups x |> cupsToGillsUs
        let litersToFluidOuncesUs x = litersToGillsUs x |> gillsUsToFluidOuncesUs
        let litersToTablespoons x = litersToFluidOuncesUs x |> fluidOuncesUsToTablespoons
        let litersToShots x = litersToTablespoons x |> tablespoonsToShots
        let litersToTeaspoons x = litersToTablespoons x |> tablespoonsToTeaspoons
        let litersToMinimsUs x = litersToTeaspoons x |> teaspoonsToMinimsUs
        let litersToHogshead x = litersToBarrels x |> barrelsToHogsheads
        let litersToPecks x = litersToGallonsUk x |> gallonsUkToPecks
        let litersToBushels x = litersToPecks x |> pecksToBushels
        let litersToQuartersUk x = litersToBushels x |> bushelsToQuarters
        let litersToQuartsUk x = litersToGallonsUk x |> gallonsUkToQuartsUk
        let litersToPintsUk x = litersToQuartsUk x |> quartsUkToPintsUk
        let litersToGillsUk x = litersToPintsUk x |> pintsUkToGillsUk
        let litersToFluidOuncesUk x = litersToGillsUk x |> gillsUkToFluidOuncesUk
        let litersToFluidDrachmsUk x = litersToFluidOuncesUk x |> fluidOuncesUkToFluidDrachmsUk
        let litersToMinimsUk x = litersToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUk

        let minimsUsToTablespoons x = minimsUsToTeaspoons x |> teaspoonsToTablespoons
        let minimsUsToFluidOuncesUs x = minimsUsToTablespoons x |> tablespoonsToFluidOuncesUs
        let minimsUsToShots x = minimsUsToTablespoons x |> tablespoonsToShots
        let minimsUsToGillsUs x = minimsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let minimsUsToCups x = minimsUsToGillsUs x |> gillsUsToCups
        let minimsUsToPintsUs x = minimsUsToCups x |> cupsToPintsUs
        let minimsUsToQuartsUs x = minimsUsToPintsUs x |> pintsUsToQuartsUs
        let minimsUsToPottles x = minimsUsToQuartsUs x |> quartsUsToPottles
        let minimsUsToGallonsUs x = minimsUsToPottles x |> pottlesToGallonsUs
        let minimsUsToCubicInches x = minimsUsToGallonsUs x |> gallonsUsToCubicInches
        let minimsUsToBarrels x = minimsUsToGallonsUs x |> gallonsUsToBarrels
        let minimsUsToLiters x = minimsUsToGallonsUs x |> gallonsUsToLiters
        let minimsUsToMilliliters x = minimsUsToLiters x |> litersToMilliliters
        let minimsUsToGallonsUk x = minimsUsToGallonsUs x |> gallonsUsToGallonsUk
        let minimsUsToPecks x = minimsUsToGallonsUk x |> gallonsUkToPecks
        let minimsUsToBushels x = minimsUsToPecks x |> pecksToBushels
        let minimsUsToQuartsUk x = minimsUsToGallonsUk x |> gallonsUkToQuartsUk
        let minimsUsToPintsUk x = minimsUsToQuartsUk x |> quartsUkToPintsUk
        let minimsUsToGillsUk x = minimsUsToPintsUk x |> pintsUkToGillsUk
        let minimsUsToFluidOuncesUk x = minimsUsToGillsUk x |> gillsUkToFluidOuncesUk
        let minimsUsToFluidDrachmsUk x = minimsUsToFluidOuncesUk x |> fluidOuncesUkToFluidDrachmsUk
        let minimsUsToMinimsUk x = minimsUsToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUk
        let minimsUsToHogshead x = minimsUsToBarrels x |> barrelsToHogsheads
        let minimsUsToQuartersUk x = minimsUsToBushels x |> bushelsToQuarters

        let minimsUkToFluidOuncesUk x = minimsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let minimsUkToGillsUk x = minimsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let minimsUkToPintsUk x = minimsUkToGillsUk x |> gillsUkToPintsUk
        let minimsUkToQuartsUk x = minimsUkToPintsUk x |> pintsUkToQuartsUk
        let minimsUkToGallonsUk x = minimsUkToQuartsUk x |> quartsUkToGallonsUk
        let minimsUkToPecks x = minimsUkToGallonsUk x |> gallonsUkToPecks
        let minimsUkToGallonsUs x = minimsUkToGallonsUk x |> gallonsUkToGallonsUs
        let minimsUkToCubicInches x = minimsUkToGallonsUs x |> gallonsUsToCubicInches
        let minimsUkToBarrels x = minimsUkToGallonsUs x |> gallonsUsToBarrels
        let minimsUkToLiters x = minimsUkToGallonsUs x |> gallonsUsToLiters
        let minimsUkToPottles x = minimsUkToGallonsUs x |> gallonsUsToPottles
        let minimsUkToQuartsUs x = minimsUkToPottles x |> pottlesToQuartsUs
        let minimsUkToPintsUs x = minimsUkToQuartsUs x |> quartsUsToPintsUs
        let minimsUkToCups x = minimsUkToPintsUs x |> pintsUsToCups
        let minimsUkToGillsUs x = minimsUkToCups x |> cupsToGillsUs
        let minimsUkToFluidOuncesUs x = minimsUkToGillsUs x |> gillsUsToFluidOuncesUs
        let minimsUkToTablespoons x = minimsUkToFluidOuncesUs x |> fluidOuncesUsToTablespoons
        let minimsUkToShots x = minimsUkToTablespoons x |> tablespoonsToShots
        let minimsUkToTeaspoons x = minimsUkToTablespoons x |> tablespoonsToTeaspoons
        let minimsUkToMinimsUs x = minimsUkToTeaspoons x |> teaspoonsToMinimsUs
        let minimsUkToFluidDramsUs x = minimsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let minimsUkToHogshead x = minimsUkToBarrels x |> barrelsToHogsheads
        let minimsUkToBushels x = minimsUkToPecks x |> pecksToBushels
        let minimsUkToQuarters x = minimsUkToBushels x |> bushelsToQuarters
        let minimsUkToMilliliters x = minimsUkToLiters x |> litersToMilliliters

        let fluidDramsUsToTeaspoons x = fluidDramsUsToMinimsUs x |> minimsUsToTeaspoons
        let fluidDramsUsToTablespoons x = fluidDramsUsToTeaspoons x |> teaspoonsToTablespoons
        let fluidDramsUsToFluidOuncesUs x = fluidDramsUsToTablespoons x |> tablespoonsToFluidOuncesUs
        let fluidDramsUsToShots x = fluidDramsUsToTablespoons x |> tablespoonsToShots
        let fluidDramsUsToGillsUs x = fluidDramsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let fluidDramsUsToCups x = fluidDramsUsToGillsUs x |> gillsUsToCups
        let fluidDramsUsToPintsUs x = fluidDramsUsToCups x |> cupsToPintsUs
        let fluidDramsUsToQuartsUs x = fluidDramsUsToPintsUs x |> pintsUsToQuartsUs
        let fluidDramsUsToPottles x = fluidDramsUsToQuartsUs x |> quartsUsToPottles
        let fluidDramsUsToGallonsUs x = fluidDramsUsToPottles x |> pottlesToGallonsUs
        let fluidDramsUsToBarrels x = fluidDramsUsToGallonsUs x |> gallonsUsToBarrels
        let fluidDramsUsToHogshead x = fluidDramsUsToBarrels x |> barrelsToHogsheads
        let fluidDramsUsToCubicInch x = fluidDramsUsToGallonsUs x |> gallonsUsToCubicInches
        let fluidDramsUsToMinimsUk x = fluidDramsUsToMinimsUs x |> minimsUsToMinimsUk
        let fluidDramsUsToFluidDrachmsUk x = fluidDramsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let fluidDramsUsToFluidOuncesUk x = fluidDramsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let fluidDramsUsToGillsUk x = fluidDramsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let fluidDramsUsToPintsUk x = fluidDramsUsToGillsUk x |> gillsUkToPintsUk
        let fluidDramsUsToQuartsUk x = fluidDramsUsToPintsUk x |> pintsUkToQuartsUk
        let fluidDramsUsToGallonsUk x = fluidDramsUsToQuartsUk x |> quartsUkToGallonsUk
        let fluidDramsUsToPecks x = fluidDramsUsToGallonsUk x |> gallonsUkToPecks
        let fluidDramsUsToBushels x = fluidDramsUsToPecks x |> pecksToBushels
        let fluidDramsUsToQuarters x = fluidDramsUsToBushels x |> bushelsToQuarters
        let fluidDramsUsToMilliliters x = fluidDramsUsToMinimsUs x |> minimsUsToMilliliters
        let fluidDramsUsToLiters x = fluidDramsUsToMilliliters x |> millilitersToLiters

        let fluidDrachmsUkToMinimsUs x = fluidDrachmsUkToMinimsUk x |> minimsUkToMinimsUs
        let fluidDrachmsUkToFluidDramsUs x = fluidDrachmsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let fluidDrachmsUkToTeaspoons x = fluidDrachmsUkToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let fluidDrachmsUkToTablespoons x = fluidDrachmsUkToTeaspoons x |> teaspoonsToTablespoons
        let fluidDrachmsUkToFluidOuncesUs x = fluidDrachmsUkToTablespoons x |> tablespoonsToFluidOuncesUs
        let fluidDrachmsUkToShots x = fluidDrachmsUkToTablespoons x |> tablespoonsToShots
        let fluidDrachmsUkToGillsUs x = fluidDrachmsUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let fluidDrachmsUkToCups x = fluidDrachmsUkToGillsUs x |> gillsUsToCups
        let fluidDrachmsUkToPintsUs x = fluidDrachmsUkToCups x |> cupsToPintsUs
        let fluidDrachmsUkToQuartsUs x = fluidDrachmsUkToPintsUs x |> pintsUsToQuartsUs
        let fluidDrachmsUkToPottles x = fluidDrachmsUkToQuartsUs x |> quartsUsToPottles
        let fluidDrachmsUkToGallonsUs x = fluidDrachmsUkToPottles x |> pottlesToGallonsUs
        let fluidDrachmsUkToBarrels x = fluidDrachmsUkToGallonsUs x |> gallonsUsToBarrels
        let fluidDrachmsUkToHogshead x = fluidDrachmsUkToBarrels x |> barrelsToHogsheads
        let fluidDrachmsUkToCubicInches x = fluidDrachmsUkToGallonsUs x |> gallonsUsToCubicInches
        let fluidDrachmsUkToGillsUk x = fluidDrachmsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let fluidDrachmsUkToPintsUk x = fluidDrachmsUkToGillsUk x |> gillsUkToPintsUk
        let fluidDrachmsUkToQuartsUk x = fluidDrachmsUkToPintsUk x |> pintsUkToQuartsUk
        let fluidDrachmsUkToGallonsUk x = fluidDrachmsUkToQuartsUk x |> quartsUkToGallonsUk
        let fluidDrachmsUkToPecks x = fluidDrachmsUkToGallonsUk x |> gallonsUkToPecks
        let fluidDrachmsUkToBushels x = fluidDrachmsUkToPecks x |> pecksToBushels
        let fluidDrachmsUkToQuarters x = fluidDrachmsUkToBushels x |> bushelsToQuarters
        let fluidDrachmsUkToMilliliters x = fluidDrachmsUkToMinimsUk x |> minimsUkToMilliliters
        let fluidDrachmsUkToLiters x = fluidDrachmsUkToMilliliters x |> millilitersToLiters

        let teaspoonsToFluidDramsUs x = teaspoonsToMinimsUs x |> minimsUsToFluidDramsUs
        let teaspoonsToFluidOuncesUs x = teaspoonsToTablespoons x |> tablespoonsToFluidOuncesUs
        let teaspoonsToShots x = teaspoonsToTablespoons x |> tablespoonsToShots
        let teaspoonsToGillsUs x = teaspoonsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let teaspoonsToCups x = teaspoonsToGillsUs x |> gillsUsToCups
        let teaspoonsToPintsUs x = teaspoonsToCups x |> cupsToPintsUs
        let teaspoonsToQuartsUs x = teaspoonsToPintsUs x |> pintsUsToQuartsUs
        let teaspoonsToPottles x = teaspoonsToQuartsUs x |> quartsUsToPottles
        let teaspoonsToGallonsUs x = teaspoonsToPottles x |> pottlesToGallonsUs
        let teaspoonsToBarrels x = teaspoonsToGallonsUs x |> gallonsUsToBarrels
        let teaspoonsToHogshead x = teaspoonsToBarrels x |> barrelsToHogsheads
        let teaspoonsToCubicInches x = teaspoonsToGallonsUs x |> gallonsUsToCubicInches
        let teaspoonsToMinimsUk x = teaspoonsToMinimsUs x |> minimsUsToMinimsUk
        let teaspoonsToFluidDrachmsUk x = teaspoonsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let teaspoonsToFluidOuncesUk x = teaspoonsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let teaspoonsToGillsUk x = teaspoonsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let teaspoonsToPintsUk x = teaspoonsToGillsUk x |> gillsUkToPintsUk
        let teaspoonsToQuartsUk x = teaspoonsToPintsUk x |> pintsUkToQuartsUk
        let teaspoonsToGallonsUk x = teaspoonsToQuartsUk x |> quartsUkToGallonsUk
        let teaspoonsToPecks x = teaspoonsToGallonsUk x |> gallonsUkToPecks
        let teaspoonsToBushels x = teaspoonsToPecks x |> pecksToBushels
        let teaspoonsToQuarters x = teaspoonsToBushels x |> bushelsToQuarters
        let teaspoonsToMilliliters x = teaspoonsToMinimsUs x |> minimsUsToMilliliters
        let teaspoonsToLiters x = teaspoonsToMilliliters x |> millilitersToLiters

        let tablespoonsToMinimsUs x = tablespoonsToTeaspoons x |> teaspoonsToMinimsUs
        let tablespoonsToFluidDramUs x = tablespoonsToMinimsUs x |> minimsUsToFluidDramsUs
        let tablespoonsToGillsUs x = tablespoonsToFluidDramUs x |> fluidDramsUsToGillsUs
        let tablespoonsToCups x = tablespoonsToGillsUs x |> gillsUsToCups
        let tablespoonsToPintsUs x = tablespoonsToCups x |> cupsToPintsUs
        let tablespoonsToQuartsUs x = tablespoonsToPintsUs x |> pintsUsToQuartsUs
        let tablespoonsToPottles x = tablespoonsToQuartsUs x |> quartsUsToPottles
        let tablespoonsToGallonsUs x = tablespoonsToPottles x |> pottlesToGallonsUs
        let tablespoonsToBarrels x = tablespoonsToGallonsUs x |> gallonsUsToBarrels
        let tablespoonsToHogsheads x = tablespoonsToBarrels x |> barrelsToHogsheads
        let tablespoonsToCubicInches x = tablespoonsToGallonsUs x |> gallonsUsToCubicInches
        let tablespoonsToMinimsUk x = tablespoonsToMinimsUs x |> minimsUsToMinimsUk
        let tablespoonsToFluidDrachmsUk x = tablespoonsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let tablespoonsToFluidOuncesUk x = tablespoonsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let tablespoonsToGillsUk x = tablespoonsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let tablespoonsToPintsUk x = tablespoonsToGillsUk x |> gillsUkToPintsUk
        let tablespoonsToQuartsUk x = tablespoonsToPintsUk x |> pintsUkToQuartsUk
        let tablespoonsToGallonsUk x = tablespoonsToQuartsUk x |> quartsUkToGallonsUk
        let tablespoonsToPecks x = tablespoonsToGallonsUk x |> gallonsUkToPecks
        let tablespoonsToBushels x = tablespoonsToPecks x |> pecksToBushels
        let tablespoonsToQuarters x = tablespoonsToBushels x |> bushelsToQuarters
        let tablespoonsToMilliliters x = tablespoonsToMinimsUs x |> minimsUsToMilliliters
        let tablespoonsToLiters x = tablespoonsToMilliliters x |> millilitersToLiters

        let fluidOuncesUsToMinimsUs x = fluidOuncesUsToTablespoons x |> tablespoonsToMinimsUs
        let fluidOuncesUsToFluidDramsUs x = fluidOuncesUsToMinimsUs x |> minimsUsToFluidDramsUs
        let fluidOuncesUsToTeaspoons x = fluidOuncesUsToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let fluidOuncesUsToShots x = fluidOuncesUsToTeaspoons x |> teaspoonsToShots
        let fluidOuncesUsToCups x = fluidOuncesUsToTablespoons x |> tablespoonsToCups
        let fluidOuncesUsToPintsUs x = fluidOuncesUsToCups x |> cupsToPintsUs
        let fluidOuncesUsToQuartsUs x = fluidOuncesUsToPintsUs x |> pintsUsToQuartsUs
        let fluidOuncesUsToPottles x = fluidOuncesUsToQuartsUs x |> quartsUsToPottles
        let fluidOuncesUsToGallonsUs x = fluidOuncesUsToPottles x |> pottlesToGallonsUs
        let fluidOuncesUsToBarrels x = fluidOuncesUsToGallonsUs x |> gallonsUsToBarrels
        let fluidOuncesUsToHogsheads x = fluidOuncesUsToBarrels x |> barrelsToHogsheads
        let fluidOuncesUsToCubicInches x = fluidOuncesUsToGallonsUs x |> gallonsUsToCubicInches
        let fluidOuncesUsToMinimsUk x = fluidOuncesUsToMinimsUs x |> minimsUsToMinimsUk
        let fluidOuncesUsToFluidDrachmsUk x = fluidOuncesUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let fluidOuncesUsToFluidOuncesUk x = fluidOuncesUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let fluidOuncesUsToGillsUk x = fluidOuncesUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let fluidOuncesUsToPintsUk x = fluidOuncesUsToGillsUk x |> gillsUkToPintsUk
        let fluidOuncesUsToQuartsUk x = fluidOuncesUsToPintsUk x |> pintsUkToQuartsUk
        let fluidOuncesUsToGallonsUk x = fluidOuncesUsToQuartsUk x |> quartsUkToGallonsUk
        let fluidOuncesUsToPecks x = fluidOuncesUsToGallonsUk x |> gallonsUkToPecks
        let fluidOuncesUsToBushels x = fluidOuncesUsToPecks x |> pecksToBushels
        let fluidOuncesUsToQuarters x = fluidOuncesUsToBushels x |> bushelsToQuarters
        let fluidOuncesUsToMilliliters x = fluidOuncesUsToMinimsUs x |> minimsUsToMilliliters
        let fluidOuncesUsToLiters x = fluidOuncesUsToMilliliters x |> millilitersToLiters

        let fluidOuncesUkToMinimsUs x = fluidOuncesUkToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUs
        let fluidOuncesUkToFluidOuncesUs x = fluidOuncesUkToMinimsUs x |> minimsUsToFluidOuncesUs
        let fluidOuncesUkToFluidDramsUs x = fluidOuncesUkToMinimsUs x |> minimsUsToFluidDramsUs
        let fluidOuncesUkToTeaspoons x = fluidOuncesUkToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let fluidOuncesUkToTablespoons x = fluidOuncesUkToTeaspoons x |> teaspoonsToTablespoons
        let fluidOuncesUkToShots x = fluidOuncesUkToTablespoons x |> tablespoonsToShots
        let fluidOuncesUkToGillsUs x = fluidOuncesUkToTablespoons x |> tablespoonsToGillsUs
        let fluidOuncesUkToCups x = fluidOuncesUkToGillsUs x |> gillsUsToCups
        let fluidOuncesUkToPintsUs x = fluidOuncesUkToCups x |> cupsToPintsUs
        let fluidOuncesUkToQuartsUs x = fluidOuncesUkToPintsUs x |> pintsUsToQuartsUs
        let fluidOuncesUkToPottles x = fluidOuncesUkToQuartsUs x |> quartsUsToPottles
        let fluidOuncesUkToGallonsUs x = fluidOuncesUkToPottles x |> pottlesToGallonsUs
        let fluidOuncesUkToBarrels x = fluidOuncesUkToGallonsUs x |> gallonsUsToBarrels
        let fluidOuncesUkToHogsheads x = fluidOuncesUkToBarrels x |> barrelsToHogsheads
        let fluidOuncesUkToCubicInches x = fluidOuncesUkToGallonsUs x |> gallonsUsToCubicInches
        let fluidOuncesUkToMinimsUk x = fluidOuncesUkToFluidDrachmsUk x |> fluidDrachmsUkToMinimsUk
        let fluidOuncesUkToPintsUk x = fluidOuncesUkToGillsUk x |> gillsUkToPintsUk
        let fluidOuncesUkToQuartsUk x = fluidOuncesUkToPintsUk x |> pintsUkToQuartsUk
        let fluidOuncesUkToGallonsUk x = fluidOuncesUkToQuartsUk x |> quartsUkToGallonsUk
        let fluidOuncesUkToPecks x = fluidOuncesUkToGallonsUk x |> gallonsUkToPecks
        let fluidOuncesUkToBushels x = fluidOuncesUkToPecks x |> pecksToBushels
        let fluidOuncesUkToQuarters x = fluidOuncesUkToBushels x |> bushelsToQuarters
        let fluidOuncesUkToMilliliters x = fluidOuncesUkToMinimsUk x |> minimsUkToMilliliters
        let fluidOuncesUkToLiters x = fluidOuncesUkToMilliliters x |> millilitersToLiters

        let gillsUsToMinimsUs x = gillsUsToFluidOuncesUs x |> fluidOuncesUsToMinimsUs
        let gillsUsToFluidDramsUs x = gillsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let gillsUsToTeaspoons x = gillsUsToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let gillsUsToTablespoons x = gillsUsToTeaspoons x |> teaspoonsToTablespoons
        let gillsUsToShots x = gillsUsToTablespoons x |> tablespoonsToShots
        let gillsUsToPintsUs x = gillsUsToCups x |> cupsToPintsUs
        let gillsUsToQuartsUs x = gillsUsToPintsUs x |> pintsUsToQuartsUs
        let gillsUsToPottles x = gillsUsToQuartsUs x |> quartsUsToPottles
        let gillsUsToGallonsUs x = gillsUsToPottles x |> pottlesToGallonsUs
        let gillsUsToBarrels x = gillsUsToGallonsUs x |> gallonsUsToBarrels
        let gillsUsToHogsheads x = gillsUsToBarrels x |> barrelsToHogsheads
        let gillsUsToCubicInches x = gillsUsToGallonsUs x |> gallonsUsToCubicInches
        let gillsUsToMinimsUk x = gillsUsToMinimsUs x |> minimsUsToMinimsUk
        let gillsUsToFluidDrachmsUk x = gillsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let gillsUsToFluidOuncesUk x = gillsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let gillsUsToGillsUk x = gillsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let gillsUsToPintsUk x = gillsUsToGillsUk x |> gillsUkToPintsUk
        let gillsUsToQuartsUk x = gillsUsToPintsUk x |> pintsUkToQuartsUk
        let gillsUsToGallonsUk x = gillsUsToQuartsUk x |> quartsUkToGallonsUk
        let gillsUsToPecks x = gillsUsToGallonsUk x |> gallonsUkToPecks
        let gillsUsToBushels x = gillsUsToPecks x |> pecksToBushels
        let gillsUsToQuarters x = gillsUsToBushels x |> bushelsToQuarters
        let gillsUsToMilliliters x = gillsUsToMinimsUs x |> minimsUsToMilliliters
        let gillsUsToLiters x = gillsUsToMilliliters x |> millilitersToLiters

        let gillsUkToMinimsUs x = gillsUkToFluidOuncesUk x |> fluidOuncesUkToMinimsUs
        let gillsUkToFluidDramsUs x = gillsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let gillsUkToTeaspoons x = gillsUkToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let gillsUkToTablespoons x = gillsUkToTeaspoons x |> teaspoonsToTablespoons
        let gillsUkToFluidOuncesUs x = gillsUkToTablespoons x |> tablespoonsToFluidOuncesUs
        let gillsUkToShots x = gillsUkToTablespoons x |> tablespoonsToShots
        let gillsUkToGillsUs x = gillsUkToTeaspoons x |> teaspoonsToGillsUs
        let gillsUkToCups x = gillsUkToGillsUs x |> gillsUsToCups
        let gillsUkToPintsUs x = gillsUkToCups x |> cupsToPintsUs
        let gillsUkToQuartsUs x = gillsUkToPintsUs x |> pintsUsToQuartsUs
        let gillsUkToPottles x = gillsUkToQuartsUs x |> quartsUsToPottles
        let gillsUkToGallonsUs x = gillsUkToPottles x |> pottlesToGallonsUs
        let gillsUkToBarrels x = gillsUkToGallonsUs x |> gallonsUsToBarrels
        let gillsUkToHogsheads x = gillsUkToBarrels x |> barrelsToHogsheads
        let gillsUkToCubicInches x = gillsUkToGallonsUs x |> gallonsUsToCubicInches
        let gillsUkToMinimsUk x = gillsUkToFluidOuncesUk x |> fluidOuncesUkToMinimsUk
        let gillsUkToFluidDrachmsUk x = gillsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let gillsUkToQuartsUk x = gillsUkToPintsUk x |> pintsUkToQuartsUk
        let gillsUkToGallonsUk x = gillsUkToQuartsUk x |> quartsUkToGallonsUk
        let gillsUkToPecks x = gillsUkToGallonsUk x |> gallonsUkToPecks
        let gillsUkToBushels x = gillsUkToPecks x |> pecksToBushels
        let gillsUkToQuarters x = gillsUkToBushels x |> bushelsToQuarters
        let gillsUkToMilliliters x = gillsUkToMinimsUk x |> minimsUkToMilliliters
        let gillsUkToLiters x = gillsUkToMilliliters x |> millilitersToLiters

        let shotsToMinimsUs x = shotsToTablespoons x |> tablespoonsToMinimsUs
        let shotsToFluidDramsUs x = shotsToMinimsUs x |> minimsUsToFluidDramsUs
        let shotsToTeaspoons x = shotsToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let shotsToFluidOunceUs x = shotsToTablespoons x |> tablespoonsToFluidOuncesUs
        let shotsToGillsUs x = shotsToFluidOunceUs x |> fluidOuncesUsToGillsUs
        let shotsToCups x = shotsToGillsUs x |> gillsUsToCups
        let shotsToPintsUs x = shotsToCups x |> cupsToPintsUs
        let shotsToQuartsUs x = shotsToPintsUs x |> pintsUsToQuartsUs
        let shotsToPottles x = shotsToQuartsUs x |> quartsUsToPottles
        let shotsToGallonsUs x = shotsToPottles x |> pottlesToGallonsUs
        let shotsToBarrels x = shotsToGallonsUs x |> gallonsUsToBarrels
        let shotsToHogsheads x = shotsToBarrels x |> barrelsToHogsheads
        let shotsToCubicInches x = shotsToGallonsUs x |> gallonsUsToCubicInches
        let shotsToMinimsUk x = shotsToMinimsUs x |> minimsUsToMinimsUk
        let shotsToFluidDrachmsUk x = shotsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let shotsToFluidOuncesUk x = shotsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let shotsToGillsUk x = shotsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let shotsToPintsUk x = shotsToGillsUk x |> gillsUkToPintsUk
        let shotsToQuartsUk x = shotsToPintsUk x |> pintsUkToQuartsUk
        let shotsToGallonsUk x = shotsToQuartsUk x |> quartsUkToGallonsUk
        let shotsToPecks x = shotsToGallonsUk x |> gallonsUkToPecks
        let shotsToBushels x = shotsToPecks x |> pecksToBushels
        let shotsToQuarters x = shotsToBushels x |> bushelsToQuarters
        let shotsToMilliliters x = shotsToMinimsUs x |> minimsUsToMilliliters
        let shotsToLiters x = shotsToMilliliters x |> millilitersToLiters

        let cupsToMinimsUs x = cupsToGillsUs x |> gillsUsToMinimsUs
        let cupsToFluidDramsUs x = cupsToMinimsUs x |> minimsUsToFluidDramsUs
        let cupsToTeaspoons x = cupsToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let cupsToTablespoons x = cupsToTeaspoons x |> teaspoonsToTablespoons
        let cupsToFluidOuncesUs x = cupsToTablespoons x |> tablespoonsToFluidOuncesUs
        let cupsToShots x = cupsToFluidOuncesUs x |> fluidOuncesUsToShots
        let cupsToQuartsUs x = cupsToPintsUs x |> pintsUsToQuartsUs
        let cupsToPottles x = cupsToQuartsUs x |> quartsUsToPottles
        let cupsToGallonsUs x = cupsToPottles x |> pottlesToGallonsUs
        let cupsToBarrels x = cupsToGallonsUs x |> gallonsUsToBarrels
        let cupsToHogsheads x = cupsToBarrels x |> barrelsToHogsheads
        let cupsToCubicInches x = cupsToGallonsUs x |> gallonsUsToCubicInches
        let cupsToMinimsUk x = cupsToMinimsUs x |> minimsUsToMinimsUk
        let cupsToFluidDrachmsUk x = cupsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let cupsToFluidOuncesUk x = cupsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let cupsToGillsUk x = cupsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let cupsToPintsUk x = cupsToGillsUk x |> gillsUkToPintsUk
        let cupsToQuartsUk x = cupsToPintsUk x |> pintsUkToQuartsUk
        let cupsToGallonsUk x = cupsToQuartsUk x |> quartsUkToGallonsUk
        let cupsToPecks x = cupsToGallonsUk x |> gallonsUkToPecks
        let cupsToBushels x = cupsToPecks x |> pecksToBushels
        let cupsToQuarters x = cupsToBushels x |> bushelsToQuarters
        let cupsToMilliliters x = cupsToMinimsUs x |> minimsUsToMilliliters
        let cupsToLiters x = cupsToMilliliters x |> millilitersToLiters

        let pintsUsToMinimsUs x = pintsUsToCups x |> cupsToMinimsUs
        let pintsUsToFluidDramsUs x = pintsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let pintsUsToTeaspoons x = pintsUsToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let pintsUsToTablespoons x = pintsUsToTeaspoons x |> teaspoonsToTablespoons
        let pintsUsToFluidOuncesUs x = pintsUsToTablespoons x |> tablespoonsToFluidOuncesUs
        let pintsUsToShots x = pintsUsToTablespoons x |> tablespoonsToShots
        let pintsUsToGillsUs x = pintsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let pintsUsToPottles x = pintsUsToQuartsUs x |> quartsUsToPottles
        let pintsUsToGallonsUs x = pintsUsToPottles x |> pottlesToGallonsUs
        let pintsUsToBarrels x = pintsUsToGallonsUs x |> gallonsUsToBarrels
        let pintsUsToHogsheads x = pintsUsToBarrels x |> barrelsToHogsheads
        let pintsUsToCubicInches x = pintsUsToGallonsUs x |> gallonsUsToCubicInches
        let pintsUsToMinimsUk x = pintsUsToMinimsUs x |> minimsUsToMinimsUk
        let pintsUsToFluidDrachmsUk x = pintsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let pintsUsToFluidOuncesUk x = pintsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let pintsUsToGillsUk x = pintsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let pintsUsToPintsUk x = pintsUsToGillsUk x |> gillsUkToPintsUk
        let pintsUsToQuartsUk x = pintsUsToPintsUk x |> pintsUkToQuartsUk
        let pintsUsToGallonsUk x = pintsUsToQuartsUk x |> quartsUkToGallonsUk
        let pintsUsToPecks x = pintsUsToGallonsUk x |> gallonsUkToPecks
        let pintsUsToBushels x = pintsUsToPecks x |> pecksToBushels
        let pintsUsToQuarters x = pintsUsToBushels x |> bushelsToQuarters
        let pintsUsToMilliliters x = pintsUsToMinimsUs x |> minimsUsToMilliliters
        let pintsUsToLiters x = pintsUsToMilliliters x |> millilitersToLiters

        let pintsUkToMinimsUs x = pintsUkToGillsUk x |> gillsUkToMinimsUs
        let pintsUkToFluidDramsUs x = pintsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let pintsUkToTeaspoons x = pintsUkToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let pintsUkToTablespoons x = pintsUkToTeaspoons x |> teaspoonsToTablespoons
        let pintsUkToFluidOuncesUs x = pintsUkToTablespoons x |> tablespoonsToFluidOuncesUs
        let pintsUkToShots x = pintsUkToFluidOuncesUs x |> fluidOuncesUsToShots
        let pintsUkToGillsUs x = pintsUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let pintsUkToCups x = pintsUkToGillsUs x |> gillsUsToCups
        let pintsUkToPintsUs x = pintsUkToCups x |> cupsToPintsUs
        let pintsUkToQuartsUs x = pintsUkToPintsUs x |> pintsUsToQuartsUs
        let pintsUkToPottles x = pintsUkToQuartsUs x |> quartsUsToPottles
        let pintsUkToGallonsUs x = pintsUkToPottles x |> pottlesToGallonsUs
        let pintsUkToBarrels x = pintsUkToGallonsUs x |> gallonsUsToBarrels
        let pintsUkToHogsheads x = pintsUkToBarrels x |> barrelsToHogsheads
        let pintsUkToCubicInches x = pintsUkToGallonsUs x |> gallonsUsToCubicInches
        let pintsUkToMinimsUk x = pintsUkToGillsUk x |> gillsUkToMinimsUk
        let pintsUkToFluidDrachmsUk x = pintsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let pintsUkToFluidOuncesUk x = pintsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let pintsUkToGallonsUk x = pintsUkToQuartsUk x |> quartsUkToGallonsUk
        let pintsUkToPecks x = pintsUkToGallonsUk x |> gallonsUkToPecks
        let pintsUkToBushels x = pintsUkToPecks x |> pecksToBushels
        let pintsUkToQuarters x = pintsUkToBushels x |> bushelsToQuarters
        let pintsUkToMilliliters x = pintsUkToMinimsUk x |> minimsUkToMilliliters
        let pintsUkToLiters x = pintsUkToMilliliters x |> millilitersToLiters

        let quartsUsToMinimsUs x = quartsUsToPintsUs x |> pintsUsToMinimsUs
        let quartsUsToFluidDramsUs x = quartsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let quartsUsToTeaspoons x = quartsUsToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let quartsUsToTablespoons x = quartsUsToTeaspoons x |> teaspoonsToTablespoons
        let quartsUsToFluidOuncesUs x = quartsUsToTablespoons x |> tablespoonsToFluidOuncesUs
        let quartsUsToShots x = quartsUsToFluidOuncesUs x |> fluidOuncesUsToShots
        let quartsUsToGillsUs x = quartsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let quartsUsToCups x = quartsUsToGillsUs x |> gillsUsToCups
        let quartsUsToGallonsUs x = quartsUsToPottles x |> pottlesToGallonsUs
        let quartsUsToBarrels x = quartsUsToGallonsUs x |> gallonsUsToBarrels
        let quartsUsToHogsheads x = quartsUsToBarrels x |> barrelsToHogsheads
        let quartsUsToCubicInches x = quartsUsToGallonsUs x |> gallonsUsToCubicInches
        let quartsUsToMinimsUk x = quartsUsToMinimsUs x |> minimsUsToMinimsUk
        let quartsUsToFluidDrachmsUk x = quartsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let quartsUsToFluidOuncesUk x = quartsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let quartsUsToGillsUk x = quartsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let quartsUsToPintsUk x = quartsUsToGillsUk x |> gillsUkToPintsUk
        let quartsUsToQuartsUk x = quartsUsToPintsUk x |> pintsUkToQuartsUk
        let quartsUsToGallonsUk x = quartsUsToQuartsUk x |> quartsUkToGallonsUk
        let quartsUsToPecks x = quartsUsToGallonsUk x |> gallonsUkToPecks
        let quartsUsToBushels x = quartsUsToPecks x |> pecksToBushels
        let quartsUsToQuarters x = quartsUsToBushels x |> bushelsToQuarters
        let quartsUsToMilliliters x = quartsUsToMinimsUs x |> minimsUsToMilliliters
        let quartsUsToLiters x = quartsUsToMilliliters x |> millilitersToLiters

        let quartsUkToMinimsUs x = quartsUkToPintsUk x |> pintsUkToMinimsUs
        let quartsUkToFluidDramsUs x = quartsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let quartsUkToTeaspoons x = quartsUkToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let quartsUkToTablespoons x = quartsUkToTeaspoons x |> teaspoonsToTablespoons
        let quartsUkToFluidOuncesUs x = quartsUkToTablespoons x |> tablespoonsToFluidOuncesUs
        let quartsUkToShots x = quartsUkToFluidOuncesUs x |> fluidOuncesUsToShots
        let quartsUkToGillsUs x = quartsUkToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let quartsUkToCups x = quartsUkToGillsUs x |> gillsUsToCups
        let quartsUkToPintsUs x = quartsUkToCups x |> cupsToPintsUs
        let quartsUkToQuartsUs x = quartsUkToPintsUs x |> pintsUsToQuartsUs
        let quartsUkToPottles x = quartsUkToQuartsUs x |> quartsUsToPottles
        let quartsUkToGallonsUs x = quartsUkToPottles x |> pottlesToGallonsUs
        let quartsUkToBarrels x = quartsUkToGallonsUs x |> gallonsUsToBarrels
        let quartsUkToHogsheads x = quartsUkToBarrels x |> barrelsToHogsheads
        let quartsUkToCubicInches x = quartsUkToGallonsUs x |> gallonsUsToCubicInches
        let quartsUkToMinimsUk x = quartsUkToPintsUk x |> pintsUkToMinimsUk
        let quartsUkToFluidDrachmsUk x = quartsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let quartsUkToFluidOuncesUk x = quartsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let quartsUkToGillsUk x = quartsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let quartsUkToPecks x = quartsUkToGallonsUk x |> gallonsUkToPecks
        let quartsUkToBushels x = quartsUkToPecks x |> pecksToBushels
        let quartsUkToQuarters x = quartsUkToBushels x |> bushelsToQuarters
        let quartsUkToMilliliters x = quartsUkToMinimsUk x |> minimsUkToMilliliters
        let quartsUkToLiters x = quartsUkToMilliliters x |> millilitersToLiters

        let pottlesToMinimsUs x = pottlesToQuartsUs x |> quartsUsToMinimsUs
        let pottlesToFluidDramsUs x = pottlesToMinimsUs x |> minimsUsToFluidDramsUs
        let pottlesToTeaspoons x = pottlesToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let pottlesToTablespoons x = pottlesToTeaspoons x |> teaspoonsToTablespoons
        let pottlesToFluidOuncesUs x = pottlesToTablespoons x |> tablespoonsToFluidOuncesUs
        let pottlesToShots x = pottlesToFluidOuncesUs x |> fluidOuncesUsToShots
        let pottlesToGillsUs x = pottlesToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let pottlesToCups x = pottlesToGillsUs x |> gillsUsToCups
        let pottlesToPintsUs x = pottlesToCups x |> cupsToPintsUs
        let pottlesToBarrels x = pottlesToGallonsUs x |> gallonsUsToBarrels
        let pottlesToHogsheads x = pottlesToBarrels x |> barrelsToHogsheads
        let pottlesToCubicInches x = pottlesToGallonsUs x |> gallonsUsToCubicInches
        let pottlesToMinimsUk x = pottlesToMinimsUs x |> minimsUsToMinimsUk
        let pottlesToFluidDrachmsUk x = pottlesToMinimsUk x |> minimsUkToFluidDrachmsUk
        let pottlesToFluidOuncesUk x = pottlesToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let pottlesToGillsUk x = pottlesToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let pottlesToPintsUk x = pottlesToGillsUk x |> gillsUkToPintsUk
        let pottlesToQuartsUk x = pottlesToPintsUk x |> pintsUkToQuartsUk
        let pottlesToGallonsUk x = pottlesToQuartsUk x |> quartsUkToGallonsUk
        let pottlesToPecks x = pottlesToGallonsUk x |> gallonsUkToPecks
        let pottlesToBushels x = pottlesToPecks x |> pecksToBushels
        let pottlesToQuarters x = pottlesToBushels x |> bushelsToQuarters
        let pottlesToMilliliters x = pottlesToMinimsUs x |> minimsUsToMilliliters
        let pottlesToLiters x = pottlesToMilliliters x |> millilitersToLiters

        let gallonsUsToMinimsUs x = gallonsUsToPottles x |> pottlesToMinimsUs
        let gallonsUsToFluidDramsUs x = gallonsUsToMinimsUs x |> minimsUsToFluidDramsUs
        let gallonsUsToTeaspoons x = gallonsUsToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let gallonsUsToTablespoons x = gallonsUsToTeaspoons x |> teaspoonsToTablespoons
        let gallonsUsToFluidOuncesUs x = gallonsUsToTablespoons x |> tablespoonsToFluidOuncesUs
        let gallonsUsToShots x = gallonsUsToFluidOuncesUs x |> fluidOuncesUsToShots
        let gallonsUsToGillsUs x = gallonsUsToFluidOuncesUs x |> fluidOuncesUsToGillsUs
        let gallonsUsToCups x = gallonsUsToGillsUs x |> gillsUsToCups
        let gallonsUsToPintsUs x = gallonsUsToCups x |> cupsToPintsUs
        let gallonsUsToQuartsUs x = gallonsUsToPintsUs x |> pintsUsToQuartsUs
        let gallonsUsToHogsheads x = gallonsUsToBarrels x |> barrelsToHogsheads
        let gallonsUsToMinimsUk x = gallonsUsToMinimsUs x |> minimsUsToMinimsUk
        let gallonsUsToFluidDrachmsUk x = gallonsUsToMinimsUk x |> minimsUkToFluidDrachmsUk
        let gallonsUsToFluidOuncesUk x = gallonsUsToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let gallonsUsToGillsUk x = gallonsUsToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let gallonsUsToPintsUk x = gallonsUsToGillsUk x |> gillsUkToPintsUk
        let gallonsUsToQuartsUk x = gallonsUsToPintsUk x |> pintsUkToQuartsUk
        let gallonsUsToPecks x = gallonsUsToGallonsUk x |> gallonsUkToPecks
        let gallonsUsToBushels x = gallonsUsToPecks x |> pecksToBushels
        let gallonsUsToQuarters x = gallonsUsToBushels x |> bushelsToQuarters
        let gallonsUsToMilliliters x = gallonsUsToLiters x |> litersToMilliliters

        let gallonsUkToMinimsUs x = gallonsUkToGallonsUs x |> gallonsUsToMinimsUs
        let gallonsUkToFluidDramsUs x = gallonsUkToMinimsUs x |> minimsUsToFluidDramsUs
        let gallonsUkToTeaspoons x = gallonsUkToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let gallonsUkToTablespoons x = gallonsUkToTeaspoons x |> teaspoonsToTablespoons
        let gallonsUkToFluidOuncesUs x = gallonsUkToTablespoons x |> tablespoonsToFluidOuncesUs
        let gallonsUkToShots x = gallonsUkToFluidOuncesUs x |> fluidOuncesUsToShots
        let gallonsUkToGillsUs x = gallonsUkToShots x |> shotsToGillsUs
        let gallonsUkToCups x = gallonsUkToGillsUs x |> gillsUsToCups
        let gallonsUkToPintsUs x = gallonsUkToCups x |> cupsToPintsUs
        let gallonsUkToQuartsUs x = gallonsUkToPintsUs x |> pintsUsToQuartsUs
        let gallonsUkToPottles x = gallonsUkToQuartsUs x |> quartsUsToPottles
        let gallonsUkToBarrels x = gallonsUkToGallonsUs x |> gallonsUsToBarrels
        let gallonsUkToHogsheads x = gallonsUkToBarrels x |> barrelsToHogsheads
        let gallonsUkToCubicInches x = gallonsUkToGallonsUs x |> gallonsUsToCubicInches
        let gallonsUkToMinimsUk x = gallonsUkToMinimsUs x |> minimsUsToMinimsUk
        let gallonsUkToFluidDrachmsUk x = gallonsUkToMinimsUk x |> minimsUkToFluidDrachmsUk
        let gallonsUkToFluidOuncesUk x = gallonsUkToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let gallonsUkToGillsUk x = gallonsUkToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let gallonsUkToPintsUk x = gallonsUkToGillsUk x |> gillsUkToPintsUk
        let gallonsUkToBushels x = gallonsUkToPecks x |> pecksToBushels
        let gallonsUkToQuarters x = gallonsUkToBushels x |> bushelsToQuarters
        let gallonsUkToMilliliters x = gallonsUkToLiters x |> litersToMilliliters

        let cubicInchesToMinimsUs x = cubicInchesToGallonsUs x |> gallonsUsToMinimsUs
        let cubicInchesToFluidDramsUs x = cubicInchesToMinimsUs x |> minimsUsToFluidDramsUs
        let cubicInchesToTeaspoons x = cubicInchesToFluidDramsUs x |> fluidDramsUsToTeaspoons
        let cubicInchesToTablespoons x = cubicInchesToTeaspoons x |> teaspoonsToTablespoons
        let cubicInchesToFluidOuncesUs x = cubicInchesToTablespoons x |> tablespoonsToFluidOuncesUs
        let cubicInchesToShots x = cubicInchesToFluidOuncesUs x |> fluidOuncesUsToShots
        let cubicInchesToGillsUs x = cubicInchesToShots x |> shotsToGillsUs
        let cubicInchesToCups x = cubicInchesToGillsUs x |> gillsUsToCups
        let cubicInchesToPintsUs x = cubicInchesToCups x |> cupsToPintsUs
        let cubicInchesToQuartsUs x = cubicInchesToPintsUs x |> pintsUsToQuartsUs
        let cubicInchesToPottles x = cubicInchesToQuartsUs x |> quartsUsToPottles
        let cubicInchesToBarrels x = cubicInchesToGallonsUs x |> gallonsUsToBarrels
        let cubicInchesToHogsheads x = cubicInchesToBarrels x |> barrelsToHogsheads
        let cubicInchesToMinimsUk x = cubicInchesToMinimsUs x |> minimsUsToMinimsUk
        let cubicInchesToFluidDrachmsUk x = cubicInchesToMinimsUk x |> minimsUkToFluidDrachmsUk
        let cubicInchesToFluidOuncesUk x = cubicInchesToFluidDrachmsUk x |> fluidDrachmsUkToFluidOuncesUk
        let cubicInchesToGillsUk x = cubicInchesToFluidOuncesUk x |> fluidOuncesUkToGillsUk
        let cubicInchesToPintsUk x = cubicInchesToGillsUk x |> gillsUkToPintsUk
        let cubicInchesToQuartsUk x = cubicInchesToPintsUk x |> pintsUkToQuartsUk
        let cubicInchesToGallonsUk x = cubicInchesToQuartsUk x |> quartsUkToGallonsUk
        let cubicInchesToPecks x = cubicInchesToGallonsUk x |> gallonsUkToPecks
        let cubicInchesToBushels x = cubicInchesToPecks x |> pecksToBushels
        let cubicInchesToQuarters x = cubicInchesToBushels x |> bushelsToQuarters
        let cubicInchesToMilliliters x = cubicInchesToMinimsUs x |> minimsUsToMilliliters
        let cubicInchesToLiters x = cubicInchesToMilliliters x |> millilitersToLiters

        let barrelsToMinimsUs = barrelsToGallonsUs >> gallonsUsToMinimsUs
        let barrelsToFluidDramsUs = barrelsToMinimsUs >> minimsUsToFluidDramsUs
        let barrelsToTeaspoons = barrelsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let barrelsToTablespoons = barrelsToTeaspoons >> teaspoonsToTablespoons
        let barrelsToFluidOuncesUs = barrelsToTablespoons >> tablespoonsToFluidOuncesUs
        let barrelsToShots = barrelsToFluidOuncesUs >> fluidOuncesUsToShots
        let barrelsToGillsUs = barrelsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let barrelsToCups = barrelsToGillsUs >> gillsUsToCups
        let barrelsToPintsUs = barrelsToCups >> cupsToPintsUs
        let barrelsToQuartsUs = barrelsToPintsUs >> pintsUsToQuartsUs
        let barrelsToPottles = barrelsToQuartsUs >> quartsUsToPottles
        let barrelsToCubicInches = barrelsToGallonsUs >> gallonsUsToCubicInches
        let barrelsToMinimsUk = barrelsToMinimsUs >> minimsUsToMinimsUk
        let barrelsToFluidDrachmsUk = barrelsToMinimsUk >> minimsUkToFluidDrachmsUk
        let barrelsToFluidOuncesUk = barrelsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let barrelsToGillsUk = barrelsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let barrelsToPintsUk = barrelsToGillsUk >> gillsUkToPintsUk
        let barrelsToQuartsUk = barrelsToPintsUk >> pintsUkToQuartsUk
        let barrelsToGallonsUk = barrelsToQuartsUk >> quartsUkToGallonsUk
        let barrelsToPecks = barrelsToGallonsUk >> gallonsUkToPecks
        let barrelsToBushels = barrelsToPecks >> pecksToBushels
        let barrelsToQuarters = barrelsToBushels >> bushelsToQuarters
        let barrelsToMilliliters = barrelsToGallonsUs >> gallonsUsToMilliliters
        let barrelsToLiters = barrelsToGallonsUs >> gallonsUsToLiters

        let hogsheadsToMinimsUs = hogsheadsToBarrels >> barrelsToMinimsUs
        let hogsheadsToFluidDramsUs = hogsheadsToMinimsUs >> minimsUsToFluidDramsUs
        let hogsheadsToTeaspoons = hogsheadsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let hogsheadsToTablespoons = hogsheadsToTeaspoons >> teaspoonsToTablespoons
        let hogsheadsToFluidOuncesUs = hogsheadsToTablespoons >> tablespoonsToFluidOuncesUs
        let hogsheadsToShots = hogsheadsToFluidOuncesUs >> fluidOuncesUsToShots
        let hogsheadsToGillsUs = hogsheadsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let hogsheadsToCups = hogsheadsToGillsUs >> gillsUsToCups
        let hogsheadsToPintsUs = hogsheadsToCups >> cupsToPintsUs
        let hogsheadsToQuartsUs = hogsheadsToPintsUs >> pintsUsToQuartsUs
        let hogsheadsToPottles = hogsheadsToQuartsUs >> quartsUsToPottles
        let hogsheadsToGallonsUs = hogsheadsToPottles >> pottlesToGallonsUs
        let hogsheadsToCubicInches = hogsheadsToGallonsUs >> gallonsUsToCubicInches
        let hogsheadsToMinimsUk = hogsheadsToMinimsUs >> minimsUsToMinimsUk
        let hogsheadsToFluidDrachmsUk = hogsheadsToMinimsUk >> minimsUkToFluidDrachmsUk
        let hogsheadsToFluidOuncesUk = hogsheadsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let hogsheadsToGillsUk = hogsheadsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let hogsheadsToPintsUk = hogsheadsToGillsUk >> gillsUkToPintsUk
        let hogsheadsToQuartsUk = hogsheadsToPintsUk >> pintsUkToQuartsUk
        let hogsheadsToGallonsUk = hogsheadsToQuartsUk >> quartsUkToGallonsUk
        let hogsheadsToPecks = hogsheadsToGallonsUk >> gallonsUkToPecks
        let hogsheadsToBushels = hogsheadsToPecks >> pecksToBushels
        let hogsheadsToQuarters = hogsheadsToBushels >> bushelsToQuarters
        let hogsheadsToMilliliters = hogsheadsToMinimsUs >> minimsUsToMilliliters
        let hogsheadsToLiters = hogsheadsToMinimsUs >> minimsUsToLiters

        let pecksToMinimsUk = pecksToGallonsUk >> gallonsUkToMinimsUk
        let pecksToMinimsUs = pecksToMinimsUk >> minimsUkToMinimsUs
        let pecksToFluidDramsUs = pecksToMinimsUs >> minimsUsToFluidDramsUs
        let pecksToTeaspoons = pecksToFluidDramsUs >> fluidDramsUsToTeaspoons
        let pecksToTablespoons = pecksToTeaspoons >> teaspoonsToTablespoons
        let pecksToFluidOuncesUs = pecksToTablespoons >> tablespoonsToFluidOuncesUs
        let pecksToShots = pecksToTablespoons >> tablespoonsToShots
        let pecksToGillsUs = pecksToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let pecksToCups = pecksToGillsUs >> gillsUsToCups
        let pecksToPintsUs = pecksToCups >> cupsToPintsUs
        let pecksToQuartsUs = pecksToPintsUs >> pintsUsToQuartsUs
        let pecksToPottles = pecksToQuartsUs >> quartsUsToPottles
        let pecksToGallonsUs = pecksToPottles >> pottlesToGallonsUs
        let pecksToBarrels = pecksToGallonsUs >> gallonsUsToBarrels
        let pecksToHogsheads = pecksToBarrels >> barrelsToHogsheads
        let pecksToCubicInches = pecksToGallonsUs >> gallonsUsToCubicInches
        let pecksToFluidDrachmsUk = pecksToMinimsUk >> minimsUkToFluidDrachmsUk
        let pecksToFluidOuncesUk = pecksToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let pecksToGillsUk = pecksToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let pecksToPintsUk = pecksToGillsUk >> gillsUkToPintsUk
        let pecksToQuartsUk = pecksToPintsUk >> pintsUkToQuartsUk
        let pecksToQuarters = pecksToBushels >> bushelsToQuarters
        let pecksToMilliliters = pecksToMinimsUs >> minimsUsToMilliliters
        let pecksToLiters = pecksToMinimsUs >> minimsUsToLiters

        let bushelsToMinimsUk = bushelsToPecks >> pecksToMinimsUk
        let bushelsToMinimsUs = bushelsToPecks >> pecksToMinimsUs
        let bushelsToFluidDramsUs = bushelsToMinimsUs >> minimsUsToFluidDramsUs
        let bushelsToTeaspoons = bushelsToFluidDramsUs >> fluidDramsUsToTeaspoons
        let bushelsToTablespoons = bushelsToTeaspoons >> teaspoonsToTablespoons
        let bushelsToFluidOuncesUs = bushelsToTablespoons >> tablespoonsToFluidOuncesUs
        let bushelsToShots = bushelsToTablespoons >> tablespoonsToShots
        let bushelsToGillsUs = bushelsToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let bushelsToCups = bushelsToGillsUs >> gillsUsToCups
        let bushelsToPintsUs = bushelsToCups >> cupsToPintsUs
        let bushelsToQuartsUs = bushelsToPintsUs >> pintsUsToQuartsUs
        let bushelsToPottles = bushelsToQuartsUs >> quartsUsToPottles
        let bushelsToGallonsUs = bushelsToPottles >> pottlesToGallonsUs
        let bushelsToBarrels = bushelsToGallonsUs >> gallonsUsToBarrels
        let bushelsToHogsheads = bushelsToBarrels >> barrelsToHogsheads
        let bushelsToCubicInches = bushelsToGallonsUs >> gallonsUsToCubicInches
        let bushelsToFluidDrachmsUk = bushelsToMinimsUk >> minimsUkToFluidDrachmsUk
        let bushelsToFluidOuncesUk = bushelsToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let bushelsToGillsUk = bushelsToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let bushelsToPintsUk = bushelsToGillsUk >> gillsUkToPintsUk
        let bushelsToQuartsUk = bushelsToPintsUk >> pintsUkToQuartsUk
        let bushelsToGallonsUk = bushelsToQuartsUk >> quartsUkToGallonsUk
        let bushelsToMilliliters = bushelsToMinimsUk >> minimsUkToMilliliters
        let bushelsToLiters = bushelsToMinimsUk >> minimsUkToLiters

        let quartersToMinimsUk = quartersToBushels >> bushelsToMinimsUk
        let quartersToMinimsUs = quartersToMinimsUk >> minimsUkToMinimsUs
        let quartersToFluidDramsUs = quartersToMinimsUs >> minimsUsToFluidDramsUs
        let quartersToTeaspoons = quartersToFluidDramsUs >> fluidDramsUsToTeaspoons
        let quartersToTablespoons = quartersToTeaspoons >> teaspoonsToTablespoons
        let quartersToFluidOuncesUs = quartersToTablespoons >> tablespoonsToFluidOuncesUs
        let quartersToShots = quartersToFluidOuncesUs >> fluidOuncesUsToShots
        let quartersToGillsUs = quartersToFluidOuncesUs >> fluidOuncesUsToGillsUs
        let quartersToCups = quartersToGillsUs >> gillsUsToCups
        let quartersToPintsUs = quartersToCups >> cupsToPintsUs
        let quartersToQuartsUs = quartersToPintsUs >> pintsUsToQuartsUs
        let quartersToPottles = quartersToQuartsUs >> quartsUsToPottles
        let quartersToGallonsUs = quartersToPottles >> pottlesToGallonsUs
        let quartersToBarrels = quartersToGallonsUs >> gallonsUsToBarrels
        let quartersToHogsheads = quartersToBarrels >> barrelsToHogsheads
        let quartersToCubicInches = quartersToGallonsUs >> gallonsUsToCubicInches
        let quartersToFluidDrachmsUk = quartersToMinimsUk >> minimsUkToFluidDrachmsUk
        let quartersToFluidOuncesUk = quartersToFluidDrachmsUk >> fluidDrachmsUkToFluidOuncesUk
        let quartersToGillsUk = quartersToFluidOuncesUk >> fluidOuncesUkToGillsUk
        let quartersToPintsUk = quartersToGillsUk >> gillsUkToPintsUk
        let quartersToQuartsUk = quartersToPintsUk >> pintsUkToQuartsUk
        let quartersToGallonsUk = quartersToQuartsUk >> quartsUkToGallonsUk
        let quartersToPecks = quartersToGallonsUk >> gallonsUkToPecks
        let quartersToMilliliters = quartersToMinimsUk >> minimsUkToMilliliters
        let quartersToLiters = quartersToMinimsUk >> minimsUkToLiters

        let removeUnit (x:decimal<_>) = decimal x

        let convertVolumeVerified ((x:decimal),fromUnit,toUnit) =
            match (fromUnit,toUnit) with
            | ("milliliters","liters") -> (millilitersToLiters(x*1m<milliliter>) |> removeUnit,"liters")
            | ("milliliters","minimsUs") -> (millilitersToMinimsUs(x*1m<milliliter>) |> removeUnit,"minimsUs")
            | ("milliliters","fluidDramsUs") -> (millilitersToFluidDramsUs(x*1m<milliliter>) |> removeUnit,"fluidDramsUs")
            | ("milliliters","teaspoons") -> (millilitersToTeaspoons(x*1m<milliliter>) |> removeUnit,"teaspoons")
            | ("milliliters","tablespoons") -> (millilitersToTablespoons(x*1m<milliliter>) |> removeUnit,"tablespoons")
            | ("milliliters","shots") -> (millilitersToShots(x*1m<milliliter>) |> removeUnit,"shots")
            | ("milliliters","fluidOuncesUs") -> (millilitersToFluidOuncesUs(x*1m<milliliter>) |> removeUnit,"fluidOuncesUs")
            | ("milliliters","gillsUs") -> (millilitersToGillsUs(x*1m<milliliter>) |> removeUnit,"gillsUs")
            | ("milliliters","cups") -> (millilitersToCups(x*1m<milliliter>) |> removeUnit,"cups")
            | ("milliliters","pintsUs") -> (millilitersToPintsUs(x*1m<milliliter>) |> removeUnit,"pintsUs")
            | ("milliliters","quartsUs") -> (millilitersToQuartsUs(x*1m<milliliter>) |> removeUnit,"quartsUs")
            | ("milliliters","pottles") -> (millilitersToPottles(x*1m<milliliter>) |> removeUnit,"pottles")
            | ("milliliters","gallonsUs") -> (millilitersToGallonsUs(x*1m<milliliter>) |> removeUnit,"gallonsUs")
            | ("milliliters","barrels") -> (millilitersToBarrels(x*1m<milliliter>) |> removeUnit,"barrels")
            | ("milliliters","hogsheads") -> (millilitersToHogsheads(x*1m<milliliter>) |> removeUnit,"hogsheads")
            | ("milliliters","cubicInches") -> (millilitersToCubicInches(x*1m<milliliter>) |> removeUnit,"cubicInches")
            | ("milliliters","minimsUk") -> (millilitersToMinimsUk(x*1m<milliliter>) |> removeUnit,"mimimsUk")
            | ("milliliters","fluidDrachmsUk") -> (millilitersToFluidDrachmsUk(x*1m<milliliter>) |> removeUnit,"fluidDrachmsUk")
            | ("milliliters","fluidOuncesUk") -> (millilitersToFluidOuncesUk(x*1m<milliliter>) |> removeUnit,"fluidOuncesUk")
            | ("milliliters","gillsUk") -> (millilitersToGillsUk(x*1m<milliliter>) |> removeUnit,"gillsUk")
            | ("milliliters","pintsUk") -> (millilitersToPintsUk(x*1m<milliliter>) |> removeUnit,"pintsUk")
            | ("milliliters","quartsUk") -> (millilitersToQuartsUk(x*1m<milliliter>) |> removeUnit,"quartsUk")
            | ("milliliters","gallonsUk") -> (millilitersToGallonsUk(x*1m<milliliter>) |> removeUnit,"gallonsUk")
            | ("milliliters","pecks") -> (millilitersToPecks(x*1m<milliliter>) |> removeUnit,"pecks")
            | ("milliliters","bushels") -> (millilitersToBushels(x*1m<milliliter>) |> removeUnit,"bushels")
            | ("milliliters","quarters") -> (millilitersToQuarters(x*1m<milliliter>) |> removeUnit,"quarters")
            | ("liters","gallonsUs") -> (litersToGallonsUs(x*1m<liter>) |> removeUnit,"gallonsUs")
            | _ -> (0m,"conversionNotImplemented")

        let convertVolume ((x:decimal),fromUnit,toUnit) =
            match List.contains fromUnit UnitsList.volumeList && List.contains toUnit UnitsList.volumeList with
            | true -> convertVolumeVerified(x,fromUnit,toUnit)
            | _ -> (0m,"unitNotSupported")

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