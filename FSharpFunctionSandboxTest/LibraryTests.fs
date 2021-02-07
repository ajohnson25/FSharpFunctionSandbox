namespace FSharpFunctions

open Xunit
open FSharpFunctions.LengthConversion
open FSharpFunctions.TemperatureConversion
open UnitsUOM.Imperial.UnitNames
open UnitsUOM.SI.UnitNames

module TemperatureConversion =
    [<Fact>]
    let ``Can convert from FtoC and CtoF`` () =
        Assert.Equal(-40m, CelsiusToFahrenheit(-40m))
        Assert.Equal(-40m,System.Math.Round(FahrenheitToCelsius(-40m),2))//Multiplied by .55 repeating so lossy

module LengthConversion =
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
    let ``International yard conversions`` () =
        Assert.Equal(1m<yard>,meterToYard(0.9144m<meter>))
        Assert.Equal(0.9144m<meter>,yardToMeter(1m<yard>))

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


