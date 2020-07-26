module LibraryTests

open System
open Xunit
open FSharpFunctionSandbox.FSharpFunctionSandbox.LengthConversion
open FSharpFunctionSandbox.FSharpFunctionSandbox.WeightConversion
open FSharpFunctionSandbox.FSharpFunctionSandbox.TemperatureConversion
open FSharpFunctionSandbox.FSharpFunctionSandbox.VolumeConversion
open Units.Imperial.UnitNames
open Units.SI.UnitNames

[<Fact>]
let ``Metric Weight convert milligram to kilogram`` () = 
    Assert.Equal(1m<gram>,mgToG(1000m<milligram>))
    Assert.Equal(1m<kilogram>,gToKg(1000m<gram>))
    Assert.Equal(1m<kilogram>,mgToKg(1000000m<milligram>))

[<Fact>]
let ``Metric Weight convert kilogram to gram`` () =
    Assert.Equal(1000m<gram>,kilogramsToGrams(1m<kilogram>))
    Assert.Equal(1000m<milligram>,gramsToMilligrams(1m<gram>))
    Assert.Equal(1000000m<milligram>,kilogramsToMilligrams(1m<kilogram>))

[<Fact>]
let ``Metric Length convert millimeter to meter`` () =
    Assert.Equal(100m<centimeter>,mmToCm(1000m<millimeter>))
    Assert.Equal(1m<meter>,cmToMeter(100m<centimeter>))
    Assert.Equal(1m<meter>,mmToMeter(1000m<millimeter>))

[<Fact>]
let ``Metric Length convert meter to millimeter`` () =
    Assert.Equal(100m<centimeter>,meterToCm(1m<meter>))
    Assert.Equal(1000m<millimeter>,cmToMm(100m<centimeter>))
    Assert.Equal(1000m<millimeter>,meterToMm(1m<meter>))

[<Fact>]
let ``Metric Milliter To Liter`` () =
    Assert.Equal(1m<liter>,milliliterToLiter(1000m<milliliter>))
    Assert.Equal(1000m<milliliter>,literToMilliliter(1m<liter>))

[<Fact>]
let ``Using ml per US Floz converts up to gallons losslessly`` () =
    Assert.Equal(1m<gallon_uk>,usGallonToUkGallon(1.201m<gallon_us>))
    Assert.Equal((3.785411784m*1000m)*1m<milliliter>,mlPerUSGallon)

[<Fact>]
let ``Metric to Imperial Length`` () =
//This conversion will always be lossy
    Assert.Equal(1.0000000000000000000000000008m<inch>,cmToInch(2.54m<centimeter>))
    Assert.Equal(1.0936132983377077865266841645m<yard>,cmToYard(100m<centimeter>))
    Assert.Equal(39370.078740157480314960629922m<thou>,cmToThou(100m<centimeter>))
    Assert.Equal(3.2808398950131233595800524935m<foot>,cmToFoot(100m<centimeter>))

[<Fact>]
let ``Imperial to Metric Length`` () =
    Assert.Equal(thouToMeter(1000m<thou>),0.0254m<meter>)
    Assert.Equal(inchToMeter(1m<inch>),0.0254m<meter>)
    Assert.Equal(footToMeter(3m<foot>),0.9144m<meter>)

[<Fact>]
let ``Imperial Weight to Kilogram`` () =
    Assert.Equal(0.45359237m<kilogram>,grainsToKilogram(7000m<grain>))
    Assert.Equal(0.45359237m<kilogram>,drachmToKilogram(256m<drachm>))
    Assert.Equal(0.45359237m<kilogram>,ouncesToKilogram(16m<ounce>))
    Assert.Equal(6.35029318m<kilogram>,stoneToKilogram(1m<stone>))
    Assert.Equal(11.33980925m<kilogram>,quartersShrtToKilogram(1m<quarter_wt_shrt>))
    Assert.Equal(12.70058636m<kilogram>,quartersLngToKilogram(1m<quarter_wt_lng>))
    Assert.Equal(45.35923700m<kilogram>,hundredweightsShrtToKilogram(1m<hundredweight_shrt>))
    Assert.Equal(50.80234544m<kilogram>,hundredweightLngToKilogram(1m<hundredweight_lng>))
    Assert.Equal(907.18474000m<kilogram>,tonsShrtToKilogram(1m<ton_shrt>))
    Assert.Equal(1016.04690880m<kilogram>,tonsLngToKilogram(1m<ton_lng>))
    Assert.Equal(0.90718474m<ton_metric>,tonsShrtToTonsMetric(1m<ton_shrt>))
    Assert.Equal(1.0160469088m<ton_metric>,tonsLongToTonsMetric(1m<ton_lng>))

[<Fact>]
let ``Imperial Weight to Gram`` () =
    Assert.Equal(1.6199727500000000000000000000m<gram>,grainsToGrams(25m<grain>))
    Assert.Equal(28.349523125000m<gram>,ouncesToGrams(1m<ounce>))
    Assert.Equal(1.7718451953125000m<gram>,drachmToGrams(1m<drachm>))
    Assert.Equal(6350.29318000m<gram>,stoneToGrams(1m<stone>))
    Assert.Equal(11339.80925000m<gram>,quartersShrtToGrams(1m<quarter_wt_shrt>))
    Assert.Equal(12700.58636000m<gram>,quartersLngToGrams(1m<quarter_wt_lng>))
    Assert.Equal(45359.23700000m<gram>,hundredweightShrtToGrams(1m<hundredweight_shrt>))
    Assert.Equal(50802.34544000m<gram>,hundredweightLngToGrams(1m<hundredweight_lng>))
    Assert.Equal(907184.74000000m<gram>,tonsShrtToGrams(1m<ton_shrt>))
    Assert.Equal(1016046.90880000m<gram>,tonsLngToGrams(1m<ton_lng>))

[<Fact>]
let ``Imperial Weight to Milligram`` () =
    Assert.Equal(64.79891m<milligram>,grainsToMilligrams(1m<grain>))
    Assert.Equal(28349.523125m<milligram>,ouncesToMilligrams(1m<ounce>))
    Assert.Equal(1771.8451953125m<milligram>,drachmToMilligrams(1m<drachm>))
    Assert.Equal(6350293.18m<milligram>,stoneToMilligrams(1m<stone>))
    Assert.Equal(11339809.25m<milligram>,quartersShrtToMilligrams(1m<quarter_wt_shrt>))
    Assert.Equal(12700586.36m<milligram>,quartersLngToMilligrams(1m<quarter_wt_lng>))
    Assert.Equal(45359237m<milligram>,hundredweightShrtToMilligrams(1m<hundredweight_shrt>))
    Assert.Equal(50802345.44m<milligram>,hundredweightLngToMilligrams(1m<hundredweight_lng>))
    Assert.Equal(907184740m<milligram>,tonsShrtToMilligrams(1m<ton_shrt>))
    Assert.Equal(1016046908.8m<milligram>,tonsLngToMilligrams(1m<ton_lng>))

[<Fact>]
let ``Imperial Weight convert up`` () =
    Assert.Equal(1m<pound>,grainsToPounds(7000m<grain>))
    Assert.Equal(1m<ounce>,drachmToOunce(16m<drachm>))
    Assert.Equal(1m<pound>,ouncesToPounds(16m<ounce>))
    Assert.Equal(1m<stone>,poundsToStone(14m<pound>))
    Assert.Equal(1m<quarter_wt_lng>,stonesToQuartersLng(2m<stone>))
    Assert.Equal(1m<quarter_wt_lng>,poundsToQuartersLng(28m<pound>))
    Assert.Equal(1m<quarter_wt_shrt>,poundsToQuartersShrt(25m<pound>))
    Assert.Equal(1m<hundredweight_lng>,quartersLngToHundredweightsLng(4m<quarter_wt_lng>))
    Assert.Equal(1m<hundredweight_shrt>,quartersShrtToHundredweightsShrt(4m<quarter_wt_shrt>))
    Assert.Equal(1m<hundredweight_shrt>,poundsToHundredweightsShrt(100m<pound>))
    Assert.Equal(1m<hundredweight_lng>,poundsToHundredweightsLng(112m<pound>))
    Assert.Equal(1m<ton_lng>,hundredweightsLngToTonsLng(20m<hundredweight_lng>))
    Assert.Equal(1m<ton_shrt>,hundredweightsShrtToTonsShrt(20m<hundredweight_shrt>))
    Assert.Equal(1m<ton_lng>,poundsToTonsLng(2240m<pound>))
    Assert.Equal(1m<ton_shrt>,poundsToTonsShrt(2000m<pound>))

[<Fact>]
let ``Imperial weight convert down`` () =
    Assert.Equal(14m<pound>,stoneToPounds(1m<stone>))
    Assert.Equal(2m<stone>,quartersLngToStones(1m<quarter_wt_lng>))
    Assert.Equal(4m<quarter_wt_lng>,hundredweightsLngToQuartersLng(1m<hundredweight_lng>))
    Assert.Equal(20m<hundredweight_lng>,tonsLngToHundredweightsLng(1m<ton_lng>))
    Assert.Equal(25m<pound>,quartersShrtToPounds(1m<quarter_wt_shrt>))
    Assert.Equal(4m<quarter_wt_shrt>,hundredweightsShrtToQuartersShrt(1m<hundredweight_shrt>))
    Assert.Equal(20m<hundredweight_shrt>,tonsShrtToHundredweightsShrt(1m<ton_shrt>))
    Assert.Equal(80m<quarter_wt_lng>,tonsLngToQuartersLng(1m<ton_lng>))
    Assert.Equal(160m<stone>,tonsLngToStones(1m<ton_lng>))

//[<Fact>]
//let ``Metric Ton(approx 2204.6lb, 1000kg)`` () =
//    Assert.Equal(0.90718474m<ton_metric>,shortTonsToMetricTons(1m<ton_shrt>))

[<Fact>]
let ``Quarters Short(25lb) to Long`` () =
    Assert.Equal(1.1160714285714285714285714286m<hundredweight_lng>,quartersShrtToHundredweightsLng(5m<quarter_wt_shrt>))
    Assert.Equal(0.8928571428571428571428571428m<quarter_wt_lng>,quartersShrtToQuartersLng(1m<quarter_wt_shrt>))
    Assert.Equal(1.1160714285714285714285714286m<ton_lng>, quartersShrtToTonsLng(100m<quarter_wt_shrt>))
    Assert.Equal(1.7857142857142857142857142856m<stone>,quartersShrtToStones(1m<quarter_wt_shrt>))

[<Fact>]
let ``Quarters Long(28lb) to Short`` () =
    Assert.Equal(1.12m<hundredweight_shrt>,quartersLngToHundredweightsShrt(4m<quarter_wt_lng>))
    Assert.Equal(1.1200m<quarter_wt_shrt>,quartersLngToQuartersShrt(1m<quarter_wt_lng>))
    Assert.Equal(0.140m<ton_shrt>,quartersLngToTonsShrt(10m<quarter_wt_lng>))

[<Fact>]
let ``Hundredweights Short(100lb) to Long`` () =
    Assert.Equal(1m<hundredweight_lng>,hundredweightsShrtToHundredweightsLng(1.12m<hundredweight_shrt>))
    Assert.Equal(3.5714285714285714285714285716m<quarter_wt_lng>,hundredweightsShrtToQuartersLng(1m<hundredweight_shrt>))
    Assert.Equal(0.9821428571428571428571428572m<ton_lng>,hundredweightsShrtToTonsLng(22m<hundredweight_shrt>))
    Assert.Equal(7.1428571428571428571428571432m<stone>,hundredweightsShrtToStones(1m<hundredweight_shrt>))

[<Fact>]
let ``Hundredweights Long(112lb) to Short`` () =
    Assert.Equal(1.12m<hundredweight_shrt>,hundredweightsLngToHundredweightsShrt(1m<hundredweight_lng>))
    Assert.Equal(4.48m<quarter_wt_shrt>, hundredweightsLngToQuartersShrt(1m<hundredweight_lng>))
    Assert.Equal(1.008m<ton_shrt>,hundredweightsLngToTonsShrt(18m<hundredweight_lng>))

[<Fact>]
let ``Tons Short(2000lb) to Long`` () = 
    Assert.Equal(17.857142857142857142857142857m<hundredweight_lng>,tonsShrtToHundredweightsLng(1m<ton_shrt>))
    Assert.Equal(71.428571428571428571428571428m<quarter_wt_lng>,tonsShrtToQuartersLng(1m<ton_shrt>))
    Assert.Equal(0.8928571428571428571428571428m<ton_lng>,tonsShrtToTonsLng(1m<ton_shrt>))
    Assert.Equal(142.85714285714285714285714286m<stone>,tonsShrtToStones(1m<ton_shrt>))

[<Fact>]
let ``Tons Long(2240lb) to Short`` () =
    Assert.Equal(89.6m<quarter_wt_shrt>,tonsLngToQuartersShrt(1m<ton_lng>))
    Assert.Equal(22.4m<hundredweight_shrt>,tonsLngToHundredweightsShrt(1m<ton_lng>))
    Assert.Equal(1.12m<ton_shrt>,tonsLngToTonsShrt(1m<ton_lng>))

[<Fact>]
let ``Stones(14lb) to Short`` () =
    Assert.Equal(1.12m<quarter_wt_shrt>,stonesToQuartersShrt(2m<stone>))
    Assert.Equal(1.12m<hundredweight_shrt>,stonesToHundredweightsShrt(8m<stone>))
    Assert.Equal(1.001m<ton_shrt>,stonesToTonsShrt(143m<stone>))
[<Fact>]
let ``International yard conversions`` () =
    Assert.Equal(1m<yard>,meterToYard(0.9144m<meter>))
    Assert.Equal(0.9144m<meter>,yardToMeter(1m<yard>))

[<Fact>]
let ``International pound conversions`` () =
    Assert.Equal(1m<pound>,kilogramsToPound(0.45359237m<kilogram>))
    Assert.Equal(0.45359237m<kilogram>,poundsToKilogram(1m<pound>))
    Assert.Equal(1m<ton_metric>,kilogramsToTonsMetric(1000m<kilogram>))

[<Fact>] 
let ``Imperial Length convert down`` () =
    Assert.Equal(3m<foot>,yardToFoot(1m<yard>))
    Assert.Equal(12m<inch>,footToInch(1m<foot>))
    Assert.Equal(1000m<thou>,inchToThou(1m<inch>))
    Assert.Equal(36m<inch>,yardToInch(1m<yard>))
    Assert.Equal(yardToThou(1m<yard>),36000m<thou>)

[<Fact>]
let ``Imperial Length convert up`` () =
    Assert.Equal((1.5m/3m)*1m<yard>,footToYard(1.5m<foot>))
    Assert.Equal((555m/1000m)*1m<inch>,thouToInch(555m<thou>))
    Assert.Equal((13m/12m)*1m<foot>,inchToFoot(13m<inch>))
    Assert.Equal(1m<foot>,thouToFoot(12000m<thou>))
    Assert.Equal(1m<yard>,thouToYard(36000m<thou>))

[<Fact>]
let ``Can convert from FtoC and CtoF`` () =
    Assert.Equal(-40m, CelsiusToFahrenheit(-40m))
    Assert.Equal(-40m,System.Math.Round(FahrenheitToCelsius(-40m),2))//Multiplied by .55 repeating so lossy