namespace FSharpFunctions

open UnitsUOM.Imperial.UnitNames
open UnitsUOM.SI.UnitNames

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
    let milligramsToStones x = milligramsToPounds x |> poundsToStones
    let milligramsToOunces x = milligramsToPounds x |> poundsToOunces
    let milligramsToDrachms x = milligramsToPounds x |> poundsToOunces |> ouncesToDrachms
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
    let metricTonsToHundredweightsLng x = metricTonsToKilograms x |> kilogramsToHundredweightsLng
    let metricTonsToHundredweightsShrt x = metricTonsToKilograms x |> kilogramsToHundredweightsShrt
    let metricTonsToTonsLng x = metricTonsToKilograms x |> kilogramsToTonsLng
    let metricTonsToTonsShrt x = metricTonsToKilograms x |> kilogramsToTonsShrt

    
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
        | ("ounces","grains") -> {amount = ouncesToGrains(x*1m<ounce>) |> removeUnit; unitName = "grains"}
        | ("ounces","drachms") -> {amount = ouncesToDrachms(x*1m<ounce>) |> removeUnit; unitName = "drachms"}
        | ("ounces","pounds") -> {amount = ouncesToPounds(x*1m<ounce>) |> removeUnit; unitName = "pounds"}
        | ("ounces","stones") -> {amount = ouncesToStones(x*1m<ounce>) |> removeUnit; unitName = "stones"}
        | ("ounces","quartersLng") -> {amount = ouncesToQuartersLng(x*1m<ounce>) |> removeUnit; unitName = "quartersLng"}
        | ("ounces","quartersShrt") -> {amount = ouncesToQuartersShrt(x*1m<ounce>) |> removeUnit; unitName = "quartersShrt"}
        | ("ounces","hundredweightsLng") -> {amount = ouncesToHundredweightsLng(x*1m<ounce>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("ounces","hundredweightsShrt") -> {amount = ouncesToHundredweightsShrt(x*1m<ounce>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("ounces","tonsLng") -> {amount = ouncesToTonsLng(x*1m<ounce>) |> removeUnit; unitName = "tonsLng"}
        | ("ounces","tonsShrt") -> {amount = ouncesToTonsShrt(x*1m<ounce>) |> removeUnit; unitName = "tonsShrt"}
        | ("ounces","tonsMetric") -> {amount = ouncesToTonsMetric(x*1m<ounce>) |> removeUnit; unitName = "tonsMetric"}
        | ("ounces","milligrams") -> {amount = ouncesToMilligrams(x*1m<ounce>) |> removeUnit; unitName = "milligrams"}
        | ("ounces","grams") -> {amount = ouncesToGrams(x*1m<ounce>) |> removeUnit; unitName = "grams"}
        | ("ounces","kilograms") -> {amount = ouncesToKilograms(x*1m<ounce>) |> removeUnit; unitName = "kilograms"}
        | ("pounds","grains") -> {amount = poundsToGrains(x*1m<pound>) |> removeUnit; unitName = "grains"}
        | ("pounds","drachms") -> {amount = poundsToDrachms(x*1m<pound>) |> removeUnit; unitName = "drachms"}
        | ("pounds","ounces") -> {amount = poundsToOunces(x*1m<pound>) |> removeUnit; unitName = "ounces"}
        | ("pounds","stones") -> {amount = poundsToStones(x*1m<pound>) |> removeUnit; unitName = "stones"}
        | ("pounds","quartersLng") -> {amount = poundsToQuartersLng(x*1m<pound>) |> removeUnit; unitName = "quartersLng"}
        | ("pounds","quartersShrt") -> {amount = poundsToQuartersShrt(x*1m<pound>) |> removeUnit; unitName = "quartersShrt"}
        | ("pounds","hundredweightsLng") -> {amount = poundsToHundredweightsLng(x*1m<pound>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("pounds","hundredweightsShrt") -> {amount = poundsToHundredweightsShrt(x*1m<pound>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("pounds","tonsLng") -> {amount = poundsToTonsLng(x*1m<pound>) |> removeUnit; unitName = "tonsLng"}
        | ("pounds","tonsShrt") -> {amount = poundsToTonsShrt(x*1m<pound>) |> removeUnit; unitName = "tonsShrt"}
        | ("pounds","tonsMetric") -> {amount = poundsToTonsMetric(x*1m<pound>) |> removeUnit; unitName = "tonsMetric"}
        | ("pounds","milligrams") -> {amount = poundsToMilligrams(x*1m<pound>) |> removeUnit; unitName = "milligrams"}
        | ("pounds","grams") -> {amount = poundsToGrams(x*1m<pound>) |> removeUnit; unitName = "grams"}
        | ("pounds","kilograms") -> {amount = poundsToKilograms(x*1m<pound>) |> removeUnit; unitName = "kilograms"}
        | ("stones","grains") -> {amount = stonesToGrains(x*1m<stone>) |> removeUnit; unitName = "grains"}
        | ("stones","drachms") -> {amount = stonesToDrachms(x*1m<stone>) |> removeUnit; unitName = "drachms"}
        | ("stones","ounces") -> {amount = stonesToOunces(x*1m<stone>) |> removeUnit; unitName = "ounces"}
        | ("stones","pounds") -> {amount = stonesToPounds(x*1m<stone>) |> removeUnit; unitName = "pounds"}
        | ("stones","quartersLng") -> {amount = stonesToQuartersLng(x*1m<stone>) |> removeUnit; unitName = "quartersLng"}
        | ("stones","quartersShrt") -> {amount = stonesToQuartersShrt(x*1m<stone>) |> removeUnit; unitName = "quartersShrt"}
        | ("stones","hundredweightsLng") -> {amount = stonesToHundredweightsLng(x*1m<stone>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("stones","hundredweightsShrt") -> {amount = stonesToHundredweightsShrt(x*1m<stone>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("stones","tonsLng") -> {amount = stonesToTonsLng(x*1m<stone>) |> removeUnit; unitName = "tonsLng"}
        | ("stones","tonsShrt") -> {amount = stonesToTonsShrt(x*1m<stone>) |> removeUnit; unitName = "tonsShrt"}
        | ("stones","tonsMetric") -> {amount = stonesToTonsMetric(x*1m<stone>) |> removeUnit; unitName = "tonsMetric"}
        | ("stones","milligrams") -> {amount = stonesToMilligrams(x*1m<stone>) |> removeUnit; unitName = "milligrams"}
        | ("stones","grams") -> {amount = stonesToGrams(x*1m<stone>) |> removeUnit; unitName = "grams"}
        | ("stones","kilograms") -> {amount = stonesToKilograms(x*1m<stone>) |> removeUnit; unitName = "kilograms"}
        | ("quartersLng","grains") -> {amount = quartersLngToGrains(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "grains"}
        | ("quartersLng","drachms") -> {amount = quartersLngToDrachms(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "drachms"}
        | ("quartersLng","ounces") -> {amount = quartersLngToOunces(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "ounces"}
        | ("quartersLng","pounds") -> {amount = quartersLngToPounds(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "pounds"}
        | ("quartersLng","stones") -> {amount = quartersLngToStones(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "stones"}
        | ("quartersLng","quartersShrt") -> {amount = quartersLngToQuartersShrt(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "quartersShrt"}
        | ("quartersLng","hundredweightsLng") -> {amount = quartersLngToHundredweightsLng(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("quartersLng","hundredweightsShrt") -> {amount = quartersLngToHundredweightsShrt(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("quartersLng","tonsLng") -> {amount = quartersLngToTonsLng(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "tonsLng"}
        | ("quartersLng","tonsShrt") -> {amount = quartersLngToTonsShrt(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "tonsShrt"}
        | ("quartersLng","tonsMetric") -> {amount = quartersLngToTonsMetric(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "tonsMetric"}
        | ("quartersLng","milligrams") -> {amount = quartersLngToMilligrams(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "milligrams"}
        | ("quartersLng","grams") -> {amount = quartersLngToGrams(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "grams"}
        | ("quartersLng","kilograms") -> {amount = quartersLngToKilograms(x*1m<quarter_wt_lng>) |> removeUnit; unitName = "kilograms"}
        | ("quartersShrt","grains") -> {amount = quartersShrtToGrains(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "grains"}
        | ("quartersShrt","drachms") -> {amount = quartersShrtToDrachms(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "drachms"}
        | ("quartersShrt","ounces") -> {amount = quartersShrtToOunces(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "ounces"}
        | ("quartersShrt","pounds") -> {amount = quartersShrtToPounds(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "pounds"}
        | ("quartersShrt","stones") -> {amount = quartersShrtToStones(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "stones"}
        | ("quartersShrt","quartersLng") -> {amount = quartersShrtToQuartersLng(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "quartersLng"}
        | ("quartersShrt","hundredweightsLng") -> {amount = quartersShrtToHundredweightsLng(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("quartersShrt","hundredweightsShrt") -> {amount = quartersShrtToHundredweightsShrt(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("quartersShrt","tonsLng") -> {amount = quartersShrtToTonsLng(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "tonsLng"}
        | ("quartersShrt","tonsShrt") -> {amount = quartersShrtToTonsShrt(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "tonsShrt"}
        | ("quartersShrt","tonsMetric") -> {amount = quartersShrtToTonsMetric(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "tonsMetric"}
        | ("quartersShrt","milligrams") -> {amount = quartersShrtToMilligrams(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "milligrams"}
        | ("quartersShrt","grams") -> {amount = quartersShrtToGrams(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "grams"}
        | ("quartersShrt","kilograms") -> {amount = quartersShrtToKilograms(x*1m<quarter_wt_shrt>) |> removeUnit; unitName = "kilograms"}
        | ("hundredweightsShrt","grains") -> {amount = hundredweightsShrtToGrains(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "grains"}
        | ("hundredweightsShrt","drachms") -> {amount = hundredweightsShrtToDrachms(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "drachms"}
        | ("hundredweightsShrt","ounces") -> {amount = hundredweightsShrtToOunces(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "ounces"}
        | ("hundredweightsShrt","pounds") -> {amount = hundredweightsShrtToPounds(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "pounds"}
        | ("hundredweightsShrt","stones") -> {amount = hundredweightsShrtToStones(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "stones"}
        | ("hundredweightsShrt","quartersLng") -> {amount = hundredweightsShrtToQuartersLng(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "quartersLng"}
        | ("hundredweightsShrt","hundredweightsLng") -> {amount = hundredweightsShrtToHundredweightsLng(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("hundredweightsShrt","quartersShrt") -> {amount = hundredweightsShrtToQuartersShrt(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "quartersShrt"}
        | ("hundredweightsShrt","tonsLng") -> {amount = hundredweightsShrtToTonsLng(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "tonsLng"}
        | ("hundredweightsShrt","tonsShrt") -> {amount = hundredweightsShrtToTonsShrt(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "tonsShrt"}
        | ("hundredweightsShrt","tonsMetric") -> {amount = hundredweightsShrtToTonsMetric(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "tonsMetric"}
        | ("hundredweightsShrt","milligrams") -> {amount = hundredweightsShrtToMilligrams(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "milligrams"}
        | ("hundredweightsShrt","grams") -> {amount = hundredweightsShrtToGrams(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "grams"}
        | ("hundredweightsShrt","kilograms") -> {amount = hundredweightsShrtToKilograms(x*1m<hundredweight_shrt>) |> removeUnit; unitName = "kilograms"}
        | ("hundredweightsLng","grains") -> {amount = hundredweightsLngToGrains(x*1m<hundredweight_lng>) |> removeUnit; unitName = "grains"}
        | ("hundredweightsLng","drachms") -> {amount = hundredweightsLngToDrachms(x*1m<hundredweight_lng>) |> removeUnit; unitName = "drachms"}
        | ("hundredweightsLng","ounces") -> {amount = hundredweightsLngToOunces(x*1m<hundredweight_lng>) |> removeUnit; unitName = "ounces"}
        | ("hundredweightsLng","pounds") -> {amount = hundredweightsLngToPounds(x*1m<hundredweight_lng>) |> removeUnit; unitName = "pounds"}
        | ("hundredweightsLng","stones") -> {amount = hundredweightsLngToStones(x*1m<hundredweight_lng>) |> removeUnit; unitName = "stones"}
        | ("hundredweightsLng","quartersLng") -> {amount = hundredweightsLngToQuartersLng(x*1m<hundredweight_lng>) |> removeUnit; unitName = "quartersLng"}
        | ("hundredweightsLng","hundredweightsShrt") -> {amount = hundredweightsLngToHundredweightsShrt(x*1m<hundredweight_lng>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("hundredweightsLng","quartersShrt") -> {amount = hundredweightsLngToQuartersShrt(x*1m<hundredweight_lng>) |> removeUnit; unitName = "quartersShrt"}
        | ("hundredweightsLng","tonsLng") -> {amount = hundredweightsLngToTonsLng(x*1m<hundredweight_lng>) |> removeUnit; unitName = "tonsLng"}
        | ("hundredweightsLng","tonsShrt") -> {amount = hundredweightsLngToTonsShrt(x*1m<hundredweight_lng>) |> removeUnit; unitName = "tonsShrt"}
        | ("hundredweightsLng","tonsMetric") -> {amount = hundredweightsLngToTonsMetric(x*1m<hundredweight_lng>) |> removeUnit; unitName = "tonsMetric"}
        | ("hundredweightsLng","milligrams") -> {amount = hundredweightsLngToMilligrams(x*1m<hundredweight_lng>) |> removeUnit; unitName = "milligrams"}
        | ("hundredweightsLng","grams") -> {amount = hundredweightsLngToGrams(x*1m<hundredweight_lng>) |> removeUnit; unitName = "grams"}
        | ("hundredweightsLng","kilograms") -> {amount = hundredweightsLngToKilograms(x*1m<hundredweight_lng>) |> removeUnit; unitName = "kilograms"}
        | ("tonsLng","grains") -> {amount = tonsLngToGrains(x*1m<ton_lng>) |> removeUnit; unitName = "grains"}
        | ("tonsLng","drachms") -> {amount = tonsLngToDrachms(x*1m<ton_lng>) |> removeUnit; unitName = "drachms"}
        | ("tonsLng","ounces") -> {amount = tonsLngToOunces(x*1m<ton_lng>) |> removeUnit; unitName = "ounces"}
        | ("tonsLng","pounds") -> {amount = tonsLngToPounds(x*1m<ton_lng>) |> removeUnit; unitName = "pounds"}
        | ("tonsLng","stones") -> {amount = tonsLngToStones(x*1m<ton_lng>) |> removeUnit; unitName = "stones"}
        | ("tonsLng","quartersLng") -> {amount = tonsLngToQuartersLng(x*1m<ton_lng>) |> removeUnit; unitName = "quartersLng"}
        | ("tonsLng","hundredweightsShrt") -> {amount = tonsLngToHundredweightsShrt(x*1m<ton_lng>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("tonsLng","quartersShrt") -> {amount = tonsLngToQuartersShrt(x*1m<ton_lng>) |> removeUnit; unitName = "quartersShrt"}
        | ("tonsLng","hundredweightsLng") -> {amount = tonsLngToHundredweightsLng(x*1m<ton_lng>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("tonsLng","tonsShrt") -> {amount = tonsLngToTonsShrt(x*1m<ton_lng>) |> removeUnit; unitName = "tonsShrt"}
        | ("tonsLng","tonsMetric") -> {amount = tonsLngToTonsMetric(x*1m<ton_lng>) |> removeUnit; unitName = "tonsMetric"}
        | ("tonsLng","milligrams") -> {amount = tonsLngToMilligrams(x*1m<ton_lng>) |> removeUnit; unitName = "milligrams"}
        | ("tonsLng","grams") -> {amount = tonsLngToGrams(x*1m<ton_lng>) |> removeUnit; unitName = "grams"}
        | ("tonsLng","kilograms") -> {amount = tonsLngToKilograms(x*1m<ton_lng>) |> removeUnit; unitName = "kilograms"}
        | ("tonsShrt","grains") -> {amount = tonsShrtToGrains(x*1m<ton_shrt>) |> removeUnit; unitName = "grains"}
        | ("tonsShrt","drachms") -> {amount = tonsShrtToDrachms(x*1m<ton_shrt>) |> removeUnit; unitName = "drachms"}
        | ("tonsShrt","ounces") -> {amount = tonsShrtToOunces(x*1m<ton_shrt>) |> removeUnit; unitName = "ounces"}
        | ("tonsShrt","pounds") -> {amount = tonsShrtToPounds(x*1m<ton_shrt>) |> removeUnit; unitName = "pounds"}
        | ("tonsShrt","stones") -> {amount = tonsShrtToStones(x*1m<ton_shrt>) |> removeUnit; unitName = "stones"}
        | ("tonsShrt","quartersLng") -> {amount = tonsShrtToQuartersLng(x*1m<ton_shrt>) |> removeUnit; unitName = "quartersLng"}
        | ("tonsShrt","hundredweightsShrt") -> {amount = tonsShrtToHundredweightsShrt(x*1m<ton_shrt>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("tonsShrt","quartersShrt") -> {amount = tonsShrtToQuartersShrt(x*1m<ton_shrt>) |> removeUnit; unitName = "quartersShrt"}
        | ("tonsShrt","hundredweightsLng") -> {amount = tonsShrtToHundredweightsLng(x*1m<ton_shrt>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("tonsShrt","tonsLng") -> {amount = tonsShrtToTonsLng(x*1m<ton_shrt>) |> removeUnit; unitName = "tonsLng"}
        | ("tonsShrt","tonsMetric") -> {amount = tonsShrtToTonsMetric(x*1m<ton_shrt>) |> removeUnit; unitName = "tonsMetric"}
        | ("tonsShrt","milligrams") -> {amount = tonsShrtToMilligrams(x*1m<ton_shrt>) |> removeUnit; unitName = "milligrams"}
        | ("tonsShrt","grams") -> {amount = tonsShrtToGrams(x*1m<ton_shrt>) |> removeUnit; unitName = "grams"}
        | ("tonsShrt","kilograms") -> {amount = tonsShrtToKilograms(x*1m<ton_shrt>) |> removeUnit; unitName = "kilograms"}
        | ("tonsMetric","grains") -> {amount = metricTonsToGrains(x*1m<ton_metric>) |> removeUnit; unitName = "grains"}
        | ("tonsMetric","drachms") -> {amount = metricTonsToDrachms(x*1m<ton_metric>) |> removeUnit; unitName = "drachms"}
        | ("tonsMetric","ounces") -> {amount = metricTonsToOunces(x*1m<ton_metric>) |> removeUnit; unitName = "ounces"}
        | ("tonsMetric","pounds") -> {amount = metricTonsToPounds(x*1m<ton_metric>) |> removeUnit; unitName = "pounds"}
        | ("tonsMetric","stones") -> {amount = metricTonsToStones(x*1m<ton_metric>) |> removeUnit; unitName = "stones"}
        | ("tonsMetric","quartersLng") -> {amount = metricTonsToQuartersLng(x*1m<ton_metric>) |> removeUnit; unitName = "quartersLng"}
        | ("tonsMetric","hundredweightsShrt") -> {amount = metricTonsToHundredweightsShrt(x*1m<ton_metric>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("tonsMetric","quartersShrt") -> {amount = metricTonsToQuartersShrt(x*1m<ton_metric>) |> removeUnit; unitName = "quartersShrt"}
        | ("tonsMetric","hundredweightsLng") -> {amount = metricTonsToHundredweightsLng(x*1m<ton_metric>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("tonsMetric","tonsLng") -> {amount = metricTonsToTonsLng(x*1m<ton_metric>) |> removeUnit; unitName = "tonsLng"}
        | ("tonsMetric","tonsShrt") -> {amount = metricTonsToTonsShrt(x*1m<ton_metric>) |> removeUnit; unitName = "tonsShrt"}
        | ("tonsMetric","milligrams") -> {amount = metricTonsToMilligrams(x*1m<ton_metric>) |> removeUnit; unitName = "milligrams"}
        | ("tonsMetric","grams") -> {amount = metricTonsToGrams(x*1m<ton_metric>) |> removeUnit; unitName = "grams"}
        | ("tonsMetric","kilograms") -> {amount = metricTonsToKilograms(x*1m<ton_metric>) |> removeUnit; unitName = "kilograms"}
        | ("milligrams","grains") -> {amount = milligramsToGrains(x*1m<milligram>) |> removeUnit; unitName = "grains"}
        | ("milligrams","drachms") -> {amount = milligramsToDrachms(x*1m<milligram>) |> removeUnit; unitName = "drachms"}
        | ("milligrams","ounces") -> {amount = milligramsToOunces(x*1m<milligram>) |> removeUnit; unitName = "ounces"}
        | ("milligrams","pounds") -> {amount = milligramsToPounds(x*1m<milligram>) |> removeUnit; unitName = "pounds"}
        | ("milligrams","stones") -> {amount = milligramsToStones(x*1m<milligram>) |> removeUnit; unitName = "stones"}
        | ("milligrams","quartersLng") -> {amount = milligramsToQuartersLng(x*1m<milligram>) |> removeUnit; unitName = "quartersLng"}
        | ("milligrams","hundredweightsShrt") -> {amount = milligramsToHundredweightsShrt(x*1m<milligram>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("milligrams","quartersShrt") -> {amount = milligramsToQuartersShrt(x*1m<milligram>) |> removeUnit; unitName = "quartersShrt"}
        | ("milligrams","hundredweightsLng") -> {amount = milligramsToHundredweightsLng(x*1m<milligram>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("milligrams","tonsLng") -> {amount = milligramsToTonsLng(x*1m<milligram>) |> removeUnit; unitName = "tonsLng"}
        | ("milligrams","tonsShrt") -> {amount = milligramsToTonsShrt(x*1m<milligram>) |> removeUnit; unitName = "tonsShrt"}
        | ("milligrams","tonsMetric") -> {amount = milligramsToTonsMetric(x*1m<milligram>) |> removeUnit; unitName = "tonsMetric"}
        | ("milligrams","grams") -> {amount = milligramsToGrams(x*1m<milligram>) |> removeUnit; unitName = "grams"}
        | ("milligrams","kilograms") -> {amount = milligramsToKilograms(x*1m<milligram>) |> removeUnit; unitName = "kilograms"}
        | ("grams","grains") -> {amount = gramsToGrains(x*1m<gram>) |> removeUnit; unitName = "grains"}
        | ("grams","drachms") -> {amount = gramsToDrachms(x*1m<gram>) |> removeUnit; unitName = "drachms"}
        | ("grams","ounces") -> {amount = gramsToOunces(x*1m<gram>) |> removeUnit; unitName = "ounces"}
        | ("grams","pounds") -> {amount = gramsToPounds(x*1m<gram>) |> removeUnit; unitName = "pounds"}
        | ("grams","stones") -> {amount = gramsToStones(x*1m<gram>) |> removeUnit; unitName = "stones"}
        | ("grams","quartersLng") -> {amount = gramsToQuartersLng(x*1m<gram>) |> removeUnit; unitName = "quartersLng"}
        | ("grams","hundredweightsShrt") -> {amount = gramsToHundredweightsShrt(x*1m<gram>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("grams","quartersShrt") -> {amount = gramsToQuartersShrt(x*1m<gram>) |> removeUnit; unitName = "quartersShrt"}
        | ("grams","hundredweightsLng") -> {amount = gramsToHundredweightsLng(x*1m<gram>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("grams","tonsLng") -> {amount = gramsToTonsLng(x*1m<gram>) |> removeUnit; unitName = "tonsLng"}
        | ("grams","tonsShrt") -> {amount = gramsToTonsShrt(x*1m<gram>) |> removeUnit; unitName = "tonsShrt"}
        | ("grams","tonsMetric") -> {amount = gramsToTonsMetric(x*1m<gram>) |> removeUnit; unitName = "tonsMetric"}
        | ("grams","milligrams") -> {amount = gramsToMilligrams(x*1m<gram>) |> removeUnit; unitName = "milligrams"}
        | ("grams","kilograms") -> {amount = gramsToKilograms(x*1m<gram>) |> removeUnit; unitName = "kilograms"}
        | ("kilograms","grains") -> {amount = kilogramsToGrains(x*1m<kilogram>) |> removeUnit; unitName = "grains"}
        | ("kilograms","drachms") -> {amount = kilogramsToDrachms(x*1m<kilogram>) |> removeUnit; unitName = "drachms"}
        | ("kilograms","ounces") -> {amount = kilogramsToOunces(x*1m<kilogram>) |> removeUnit; unitName = "ounces"}
        | ("kilograms","pounds") -> {amount = kilogramsToPounds(x*1m<kilogram>) |> removeUnit; unitName = "pounds"}
        | ("kilograms","stones") -> {amount = kilogramsToStones(x*1m<kilogram>) |> removeUnit; unitName = "stones"}
        | ("kilograms","quartersLng") -> {amount = kilogramsToQuartersLng(x*1m<kilogram>) |> removeUnit; unitName = "quartersLng"}
        | ("kilograms","hundredweightsShrt") -> {amount = kilogramsToHundredweightsShrt(x*1m<kilogram>) |> removeUnit; unitName = "hundredweightsShrt"}
        | ("kilograms","quartersShrt") -> {amount = kilogramsToQuartersShrt(x*1m<kilogram>) |> removeUnit; unitName = "quartersShrt"}
        | ("kilograms","hundredweightsLng") -> {amount = kilogramsToHundredweightsLng(x*1m<kilogram>) |> removeUnit; unitName = "hundredweightsLng"}
        | ("kilograms","tonsLng") -> {amount = kilogramsToTonsLng(x*1m<kilogram>) |> removeUnit; unitName = "tonsLng"}
        | ("kilograms","tonsShrt") -> {amount = kilogramsToTonsShrt(x*1m<kilogram>) |> removeUnit; unitName = "tonsShrt"}
        | ("kilograms","tonsMetric") -> {amount = kilogramsToTonsMetric(x*1m<kilogram>) |> removeUnit; unitName = "tonsMetric"}
        | ("kilograms","milligrams") -> {amount = kilogramsToMilligrams(x*1m<kilogram>) |> removeUnit; unitName = "milligrams"}
        | ("kilograms","grams") -> {amount = kilogramsToGrams(x*1m<kilogram>) |> removeUnit; unitName = "grams"}

        | _ -> {amount = 0m; unitName = "conversionNotImplemented"}

    let convertWeight ((x:decimal),fromUnit,toUnit) =
        match List.contains fromUnit UnitsList.weightList && List.contains toUnit UnitsList.weightList with
        | true -> convertWeightVerified(x,fromUnit,toUnit)
        | _ -> {amount = 0m; unitName = "unitNotSupported"}