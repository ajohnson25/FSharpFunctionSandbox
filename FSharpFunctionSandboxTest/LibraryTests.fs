module LibraryTests

open System
open Xunit
open FSharpFunctionSandbox.FSharpFunctionSandbox.MeasureConversion
open FSharpFunctionSandbox.FSharpFunctionSandbox.TemperatureConversion
open Units.Imperial.Name
open Units.SIUF.Name


[<Fact>]
let ``Metric Weight convert up`` () = 
    Assert.Equal(mgToG(1000m<milligram>),1m<gram>)
    Assert.Equal(gToKg(1000m<gram>),1m<kilogram>)
    Assert.Equal(mgToKg(1000000m<milligram>),1m<kilogram>)

[<Fact>]
let ``Metric Weight convert down`` () =
    Assert.Equal(kgToG(1m<kilogram>),1000m<gram>)
    Assert.Equal(gToMg(1m<gram>),1000m<milligram>)
    Assert.Equal(kgToMg(1m<kilogram>),1000000m<milligram>)

[<Fact>]
let ``Metric Length convert up`` () =
    Assert.Equal(mmToCm(1000m<millimeter>),100m<centimeter>)
    Assert.Equal(cmToMeter(100m<centimeter>),1m<meter>)
    Assert.Equal(mmToMeter(1000m<millimeter>),1m<meter>)

[<Fact>]
let ``Metric Length convert down`` () =
    Assert.Equal(meterToCm(1m<meter>),100m<centimeter>)
    Assert.Equal(cmToMm(100m<centimeter>),1000m<millimeter>)
    Assert.Equal(meterToMm(1m<meter>),1000m<millimeter>)

[<Fact>]
let ``Metric to Imperial`` () =
//This conversion will always be lossy
    Assert.Equal(cmToIn(100m<centimeter>),(100m / 2.54m)*1m<inch>)
    Assert.Equal(cmToThou(100m<centimeter>),39400m<thou>)
    Assert.Equal(cmToFoot(100m<centimeter>),(100m / 30.48m)*1m<foot>)

[<Fact>] 
let ``Imperial Length convert down`` () =
    Assert.Equal(yardToFeet(1m<yard>),3m<foot>)
    Assert.Equal(footToInch(1m<foot>),12m<inch>)
    Assert.Equal(inchToThou(1m<inch>),1000m<thou>)
    Assert.Equal(yardToInch(1m<yard>),36m<inch>)
    Assert.Equal(yardToThou(1m<yard>),36000m<thou>)

[<Fact>]
let ``Imperial Length convert up`` () =
    Assert.Equal(footToYard(1.5m<foot>), (1.5m/3m)*1m<yard>)
    Assert.Equal(thouToInch(555m<thou>), (555m/1000m)*1m<inch>)
    Assert.Equal(inchToFoot(13m<inch>), (13m/12m)*1m<foot>)
    Assert.Equal(thouToFoot(12000m<thou>),1m<foot>)
    Assert.Equal(thouToYard(36000m<thou>),1m<yard>)

[<Fact>]
let ``Imperial conversions give different results`` () =
    //Converting from cm to imperial will provide different values
    Assert.NotEqual(inchToThou(cmToIn(100m<centimeter>)),cmToThou(100m<centimeter>)) //Off by 30-31 thou

[<Fact>]
let ``Can convert from FtoC and CtoF`` () =
    Assert.Equal(-40m, CelsiusToFahrenheit(-40m))
    Assert.Equal(-40m,System.Math.Round(FahrenheitToCelsius(-40m),2))//Multiplied by .55 repeating so lossy