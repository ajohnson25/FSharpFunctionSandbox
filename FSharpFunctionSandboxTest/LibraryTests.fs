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
    Assert.Equal(1000m<gram>,kgToG(1m<kilogram>))
    Assert.Equal(1000m<milligram>,gToMg(1m<gram>))
    Assert.Equal(1000000m<milligram>,kgToMg(1m<kilogram>))

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
let ``Metric to Imperial`` () =
//This conversion will always be lossy
    Assert.Equal(1.0000000000000000000000000008m<inch>,cmToInch(2.54m<centimeter>))
    Assert.Equal(1.0936132983377077865266841645m<yard>,cmToYard(100m<centimeter>))
    Assert.Equal(39370.078740157480314960629922m<thou>,cmToThou(100m<centimeter>))
    Assert.Equal(3.2808398950131233595800524935m<foot>,cmToFoot(100m<centimeter>))

[<Fact>]
let ``Imperial to Metric`` () =
    Assert.Equal(thouToMeter(1000m<thou>),0.0254m<meter>)
    Assert.Equal(inchToMeter(1m<inch>),0.0254m<meter>)
    Assert.Equal(footToMeter(3m<foot>),0.9144m<meter>)


[<Fact>] 
let ``Imperial Length convert down`` () =
    Assert.Equal(yardToFoot(1m<yard>),3m<foot>)
    Assert.Equal(footToInch(1m<foot>),12m<inch>)
    Assert.Equal(inchToThou(1m<inch>),1000m<thou>)
    Assert.Equal(yardToInch(1m<yard>),36m<inch>)
    Assert.Equal(yardToThou(1m<yard>),36000m<thou>)

[<Fact>]
let ``Imperial Length convert up`` () =
    Assert.Equal((1.5m/3m)*1m<yard>,footToYard(1.5m<foot>))
    Assert.Equal((555m/1000m)*1m<inch>,thouToInch(555m<thou>))
    Assert.Equal((13m/12m)*1m<foot>,inchToFoot(13m<inch>))
    Assert.Equal(1m<foot>,thouToFoot(12000m<thou>))
    Assert.Equal(1m<yard>,thouToYard(36000m<thou>))

[<Fact>]
let ``Imperial Weight convert up`` () =
    Assert.Equal(1m<pound>,grainsToPounds(7000m<grain>))
    Assert.Equal(1m<ounce>,drachmToOunce(16m<drachm>))
    Assert.Equal(1m<pound>,ouncesToPounds(16m<ounce>))
    Assert.Equal(1m<stone>,poundsToStone(14m<pound>))
    Assert.Equal(1m<quarter_wt>,stonesToQuarter(2m<stone>))
    Assert.Equal(1m<quarter_wt>,poundsToQuarter(28m<pound>))
    Assert.Equal(1m<hundredweight>,quartersToHundredweight(4m<quarter_wt>))
    Assert.Equal(1m<hundredweight>,poundsToHundredweight(112m<pound>))
    Assert.Equal(1m<ton>,hundredweightToTon(20m<hundredweight>))
    Assert.Equal(1m<ton>,poundsToTon(2240m<pound>))

[<Fact>]
let ``International yard and pound conversions`` () =
    Assert.Equal(1m<yard>,meterToYard(0.9144m<meter>))
    Assert.Equal(0.9144m<meter>,yardToMeter(1m<yard>))

    Assert.Equal(1m<pound>,kilogramToPound(0.45359237m<kilogram>))
    Assert.Equal(0.45359237m<kilogram>,poundToKilogram(1m<pound>))


[<Fact>]
let ``Can convert from FtoC and CtoF`` () =
    Assert.Equal(-40m, CelsiusToFahrenheit(-40m))
    Assert.Equal(-40m,System.Math.Round(FahrenheitToCelsius(-40m),2))//Multiplied by .55 repeating so lossy