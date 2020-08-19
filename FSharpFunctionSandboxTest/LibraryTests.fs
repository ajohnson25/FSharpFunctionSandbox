namespace Functions

open System
open Xunit
open FSharpFunctionSandbox.FSharpFunctionSandbox.LengthConversion
open FSharpFunctionSandbox.FSharpFunctionSandbox.WeightConversion
open FSharpFunctionSandbox.FSharpFunctionSandbox.TemperatureConversion
open FSharpFunctionSandbox.FSharpFunctionSandbox.VolumeConversion
open Units.Imperial.UnitNames
open Units.SI.UnitNames

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

module VolumeConversion =

    [<Fact>]
    let ``Using ml per US Floz converts up to gallons losslessly`` () =
        Assert.Equal(1m<gallon_uk>,usGallonsToUkGallons(1.201m<gallon_us>))
        Assert.Equal(3.785411784m<liter>,gallonsUsToLiters(1m<gallon_us>))
        Assert.Equal(4.54609m<liter>,gallonsUkToLiters(1m<gallon_uk>))

    [<Fact>]
    let ``Volume US`` () =
        Assert.Equal(60m<minim_us>,fluidDramsToMinimsUs(1m<fluidDram_us>))
        Assert.Equal(80m<minim_us>,teaspoonsUsToMinimsUs(1m<teaspoon>))
        Assert.Equal(1m<tablespoon>,teaspoonsToTablespoons(3m<teaspoon>))
        Assert.Equal(3m<teaspoon>,tablespoonsToTeaspoons(1m<tablespoon>))
        Assert.Equal(2m<tablespoon>,fluidOuncesUsToTablespoons(1m<fluidOunce_us>))
        Assert.Equal(1m<fluidOunce_us>,tablespoonsToFluidOuncesUs(2m<tablespoon>))
        Assert.Equal(1m<shot>,tablespoonsToShots(3m<tablespoon>))
        Assert.Equal(3m<tablespoon>,shotsToTablespoons(1m<shot>))
        Assert.Equal(1m<gill_us>,fluidOuncesUsToGillsUs(4m<fluidOunce_us>))
        Assert.Equal(4m<fluidOunce_us>,gillsUsToFluidOuncesUs(1m<gill_us>))
        Assert.Equal(2m<gill_us>,cupsToGillsUs(1m<cup>))
        Assert.Equal(1m<cup>,gillsUsToCups(2m<gill_us>))
        Assert.Equal(1m<pint_us>,cupsToPintsUs(2m<cup>))
        Assert.Equal(2m<cup>,pintsUsToCups(1m<pint_us>))
        Assert.Equal(1m<quart_us>,pintsUsToQuartsUs(2m<pint_us>))
        Assert.Equal(2m<pint_us>,QuartsUsToPintsUs(1m<quart_us>))
        Assert.Equal(2m<quart_us>,pottlesToQuartsUs(1m<pottle>))
        Assert.Equal(1m<pottle>,quartsUsToPottles(2m<quart_us>))
        Assert.Equal(1m<gallon_us>,pottlesToGallonsUs(2m<pottle>))
        Assert.Equal(2m<pottle>,gallonsUsToPottles(1m<gallon_us>))
        Assert.Equal(31.5m<gallon_us>,barrelsToGallonsUs(1m<barrel>))
        Assert.Equal(1m<barrel>,gallonsUsToBarrels(31.5m<gallon_us>))
        Assert.Equal(1m<hogshead>,barrelsToHogshead(2m<barrel>))
        Assert.Equal(2m<barrel>,hogsheadToBarrels(1m<hogshead>))

        Assert.Equal(1m<gallon_us>,cubicInchesToGallonsUs(231m<cubicInch>))
        Assert.Equal(231m<cubicInch>,gallonsUsToCubicInches(1m<gallon_us>))

        Assert.Equal(3.785411784m<liter>,gallonsUsToLiters(1m<gallon_us>))
        

    [<Fact>]
    let ``Volume UK`` () = 
        Assert.Equal(1m<fluidDrachm_uk>,minimsUkToFluidDrachmsUk(60m<minim_uk>))
        Assert.Equal(60m<minim_uk>,fluidDrachmsUkToMinimsUk(1m<fluidDrachm_uk>))
        Assert.Equal(1m<fluidOunce_uk>,fluidDrachmUkToFluidOuncesUk(8m<fluidDrachm_uk>))
        Assert.Equal(8m<fluidDrachm_uk>,fluidOuncesUkToFluidDrachmsUk(1m<fluidOunce_uk>))
        Assert.Equal(4m<gill_uk>,pintsUkToGillsUk(1m<pint_uk>))
        Assert.Equal(1m<pint_uk>,gillsUkToPintsUk(4m<gill_uk>))
        Assert.Equal(2m<pint_uk>,quartsUkToPintsUk(1m<quart_uk>))
        Assert.Equal(1m<quart_uk>,pintsUkToQuartsUk(2m<pint_uk>))
        Assert.Equal(1m<gallon_uk>,quartsUkToGallonsUk(4m<quart_uk>))
        Assert.Equal(4m<quart_uk>,gallonsUkToQuartsUk(1m<gallon_uk>))
        Assert.Equal(2m<gallon_uk>,pecksUkToGallonsUk(1m<peck>))
        Assert.Equal(1m<peck>,gallonsUkToPecks(2m<gallon_uk>))
        Assert.Equal(1m<bushel>,pecksToBushels(4m<peck>))
        Assert.Equal(8m<bushel>,quartersUkToBushels(1m<quarter_vl>))
        Assert.Equal(1m<quarter_vl>,bushelsToQuartersUk(8m<bushel>))
        Assert.Equal(1m<gallon_uk>,litersToGallonsUk(4.54609m<liter>))
        Assert.Equal(4.54609m<liter>,gallonsUkToLiters(1m<gallon_uk>))
        Assert.Equal(1m<gallon_uk>,litersToGallonsUk(4.54609m<liter>))
        Assert.Equal(1.201m<gallon_us>,gallonsUkToGallonsUs(1m<gallon_uk>))

    [<Fact>]
    let ``Milliliter to Other`` () =
         Assert.Equal(1m<liter>,millilitersToLiters(1000m<milliliter>))
         Assert.Equal(0.9999995287170585930632269623m<gallon_us>,millilitersToGallonsUs(3785.41m<milliliter>))
         Assert.Equal(1.0000021704375821745473807612m<pottle>,millilitersToPottles(1892.71m<milliliter>))
         Assert.Equal(1.0000000570611633093600577220m<quart_us>,millilitersToQuartsUs(946.353m<milliliter>))
         Assert.Equal(0.9999990003729538767663962024m<pint_us>,millilitersToPintsUs(473.176m<milliliter>))
         Assert.Equal(0.9999990003729538767663962032m<cup>,millilitersToCups(236.588m<milliliter>))
         Assert.Equal(0.9999990003729538767663962016m<gill_us>,millilitersToGillsUs(118.294m<milliliter>))
         Assert.Equal(0.9998806512934974262763060096m<fluidOunce_us>,millilitersToFluidOuncesUs(29.57m<milliliter>))
         Assert.Equal(1.0000023817752240610661130752m<tablespoon>,millilitersToTablespoons(14.7868m<milliliter>))
         Assert.Equal(1.0000001275070439381996351573m<shot>,millilitersToShots(44.3603m<milliliter>))
         Assert.Equal(0.9999996766534079136263395584m<teaspoon>,millilitersToTeaspoons(4.92892m<milliliter>))
         Assert.Equal(0.9999996766534079136263393280m<minim_us>,millilitersToMinimsUs(0.0616115m<milliliter>))
         Assert.Equal(1m<fluidDram_us>,millilitersToFluidDramsUs(3.6966911953125m<milliliter>))
         Assert.Equal(1m<barrel>,millilitersToBarrels(119240.471196m<milliliter>))
         Assert.Equal(1.0000000042267528377303746461m<hogshead>,millilitersToHogshead(238480.9434m<milliliter>))
         Assert.Equal(1.000002196854787410362222298m<cubicInch>,millilitersToCubicInches(16.3871m<milliliter>))
         Assert.Equal(1m<gallon_uk>,millilitersToGallonsUk(4546.09m<milliliter>))
         Assert.Equal(1m<peck>,millilitersToPecks(9092.18m<milliliter>))
         Assert.Equal(0.9999978003075170091221247268m<quart_uk>,millilitersToQuartsUk(1136.52m<milliliter>))
         Assert.Equal(0.9999995600615034018244249456m<pint_uk>,millilitersToPintsUk(568.261m<milliliter>))
         Assert.Equal(1.0000329953872448631681290944m<gill_uk>,millilitersToGillsUk(142.07m<milliliter>))
         Assert.Equal(0.9999994500768792522805311818m<bushel>,millilitersToBushelsUk(36368.7m<milliliter>))
         Assert.Equal(1m<quarter_vl>,millilitersToQuartersUk(290949.76m<milliliter>))
         Assert.Equal(0.9998922150683334469841116160m<fluidOunce_uk>,millilitersToFluidOuncesUk(28.41m<milliliter>))
         Assert.Equal(1m<fluidDrachm_uk>,millilitersToFluidDrachmsUk(3.5516328125m<milliliter>))
         Assert.Equal(1.0000003343532574146134400000m<minim_uk>,millilitersToMinimsUk(0.0591939m<milliliter>))

    [<Fact>]
    let ``Liter to Other`` () =
        Assert.Equal(1m<gallon_us>,litersToGallonsUs(3.785411784m<liter>))
        Assert.Equal(1000m<milliliter>,litersToMilliliters(1m<liter>))
        Assert.Equal(1m<gallon_us>,litersToGallonsUs(3.785411784m<liter>))
        Assert.Equal(1m<gallon_uk>,litersToGallonsUk(4.54609m<liter>))
        Assert.Equal(1.0000002415622792420351414795m<barrel>,litersToBarrels(119.2405m<liter>))
        Assert.Equal(61.023744094732283952756881890m<cubicInch>,litersToCubicInches(1m<liter>))
        Assert.Equal(1.0000021704375821745473807612m<pottle>,litersToPottles(1.89271m<liter>))
        Assert.Equal(1.0566882094325936615195996864m<quart_us>,litersToQuartsUs(1m<liter>))
        Assert.Equal(2.1133764188651873230391993728m<pint_us>,litersToPintsUs(1m<liter>))
        Assert.Equal(4.2267528377303746460783987456m<cup>,litersToCups(1m<liter>))
        Assert.Equal(8.453505675460749292156797491m<gill_us>,litersToGillsUs(1m<liter>))
        Assert.Equal(33.814022701842997168627189964m<fluidOunce_us>,litersToFluidOuncesUs(1m<liter>))
        Assert.Equal(67.628045403685994337254379928m<tablespoon>,litersToTablespoons(1m<liter>))
        Assert.Equal(22.542681801228664779084793309m<shot>,litersToShots(1m<liter>))
        Assert.Equal(202.88413621105798301176313978m<teaspoon>,litersToTeaspoons(1m<liter>))
        Assert.Equal(16230.730896884638640941051182m<minim_us>,litersToMinimsUs(1m<liter>))
        Assert.Equal(1.0000002415622792420351414794m<hogshead>,litersToHogshead(238.481m<liter>))
        Assert.Equal(0.9689392422939273089622070834m<peck>,litersToPecks(8.80977m<liter>))
        Assert.Equal(0.9689397922170480566816759017m<bushel>,litersToBushels(35.2391m<liter>))
        Assert.Equal(1.0000008248846811215792032274m<quarter_vl>,litersToQuartersUk(290.95m<liter>))
        Assert.Equal(0.8798769931963511501092147316m<quart_uk>,litersToQuartsUk(1m<liter>))
        Assert.Equal(1.7597539863927023002184294632m<pint_uk>,litersToPintsUk(1m<liter>))
        Assert.Equal(7.0390159455708092008737178528m<gill_uk>,litersToGillsUk(1m<liter>))
        Assert.Equal(35.195079727854046004368589264m<fluidOunce_uk>,litersToFluidOuncesUk(1m<liter>))
        Assert.Equal(281.56063782283236803494871411m<fluidDrachm_uk>,litersToFluidDrachmsUk(1m<liter>))
        Assert.Equal(16893.638269369942082096922847m<minim_uk>,litersToMinimsUk(1m<liter>))

    [<Fact>]
    let ``Minims US to Other``() = 
        Assert.Equal(1m<fluidDram_us>,minimsUsToFluidDramsUs(60m<minim_us>))
        Assert.Equal(1m<teaspoon>,minimsUsToTeaspoons(80m<minim_us>))
        Assert.Equal(1m<tablespoon>,minimsUsToTablespoons(240m<minim_us>))
        Assert.Equal(1m<fluidOunce_us>,minimsUsToFluidOuncesUs(480m<minim_us>))
        Assert.Equal(1m<shot>,minimsUsToShots(720m<minim_us>))
        Assert.Equal(1m<gill_us>,minimsUsToGillsUs(1920m<minim_us>))
        Assert.Equal(1m<cup>,minimsUsToCups(3840m<minim_us>))
        Assert.Equal(1m<pint_us>,minimsUsToPintsUs(7680m<minim_us>))
        Assert.Equal(1m<quart_us>,minimsUsToQuartsUs(15360m<minim_us>))
        Assert.Equal(1m<pottle>,minimsUsToPottles(30720m<minim_us>))
        Assert.Equal(1m<gallon_us>,minimsUsToGallonsUs(61440m<minim_us>))
        Assert.Equal(0.99999990234375m<cubicInch>,minimsUsToCubicInches(265.974m<minim_us>))
        Assert.Equal(1.0000005166997354497354497356m<barrel>,minimsUsToBarrels(1935361m<minim_us>))
        Assert.Equal(0.9999980963959765625000000001m<liter>,minimsUsToLiters(16230.7m<minim_us>))
        Assert.Equal(0.9999980963959765625000001000m<milliliter>,minimsUsToMilliliters(16.2307m<minim_us>))
        Assert.Equal(0.9999588016930335831251734664m<gallon_uk>,minimsUsToGallonsUk(73786.4m<minim_us>))
        Assert.Equal(0.9688988017797668609492089925m<peck>,minimsUsToPecks(142989m<minim_us>))
        Assert.Equal(0.9688988017797668609492089925m<bushel>,minimsUsToBushels(571956m<minim_us>))
        Assert.Equal(0.9999588016930335831251734664m<quart_uk>,minimsUsToQuartsUk(18446.6m<minim_us>))
        Assert.Equal(0.9999588016930335831251734664m<pint_uk>,minimsUsToPintsUk(9223.3m<minim_us>))
        Assert.Equal(0.9999566333610879822370247008m<gill_uk>,minimsUsToGillsUk(2305.82m<minim_us>))
        Assert.Equal(0.9999479600333055786844296480m<fluidOunce_uk>,minimsUsToFluidOuncesUk(461.16m<minim_us>))
        Assert.Equal(1.0407993338884263114071607040m<fluidDrachm_uk>,minimsUsToFluidDrachmsUk(60m<minim_us>))
        Assert.Equal(1.0407993338884263114071654400m<minim_uk>,minimsUsToMinimsUk(1m<minim_us>))
        Assert.Equal(0.9999583017724504346421384956m<quarter_vl>,minimsUsToQuartersUk(4722327.239112968m<minim_us>))

module WeightConversion =
    [<Fact>]
    let ``Metric Ton(1000kg, approx 2204.6lb)`` () =
        Assert.Equal(1000000m<gram>,metricTonsToGrams(1m<ton_metric>))
        Assert.Equal(1000m<kilogram>,metricTonsToKilograms(1m<ton_metric>))
        Assert.Equal(1000000000m<milligram>,metricTonsToMilligrams(1m<ton_metric>))
        Assert.Equal(15432358.352941430650608166094m<grain>,metricTonsToGrains(1m<ton_metric>))
        Assert.Equal(564383.39119328660665081293146m<drachm>,metricTonsToDrachms(1m<ton_metric>))
        Assert.Equal(35273.961949580412915675808216m<ounce>,metricTonsToOunces(1m<ton_metric>))
        Assert.Equal(2204.6226218487758072297380135m<pound>,metricTonsToPounds(1m<ton_metric>))
        Assert.Equal(157.47304441776970051640985811m<stone>,metricTonsToStones(1m<ton_metric>))
        Assert.Equal(88.18490487395103228918952054m<quarter_wt_shrt>,metricTonsToQuartersShrt(1m<ton_metric>))
        Assert.Equal(78.736522208884850258204929055m<quarter_wt_lng>,metricTonsToQuartersLng(1m<ton_metric>))
        Assert.Equal(22.046226218487758072297380135m<hundredweight_shrt>,metricTonsToHundredweightShrt(1m<ton_metric>))
        Assert.Equal(19.684130552221212564551232264m<hundredweight_lng>,metricTonsToHundredweightLng(1m<ton_metric>))
        Assert.Equal(0.9842065276110606282275616132m<ton_lng>,metricTonsToTonLng(1m<ton_metric>))
        Assert.Equal(1.1023113109243879036148690068m<ton_shrt>,metricTonsToTonShrt(1m<ton_metric>))

    [<Fact>]
    let ``Milligrams(1/1000g) to Other`` () =
        Assert.Equal(1m<gram>,milligramsToGrams(1000m<milligram>))
        Assert.Equal(1m<kilogram>,milligramsToKilograms(1000000m<milligram>))
        Assert.Equal(1m<ton_metric>,milligramsToTonsMetric(1000000000m<milligram>))
        Assert.Equal(1.0000013889122517647287586000m<grain>,milligramsToGrains(64.7990m<milligram>))        
        Assert.Equal(1.0000873691945038669852405248m<drachm>,milligramsToDrachms(1772m<milligram>))
        Assert.Equal(1.0000168212706047061594091632m<ounce>,milligramsToOunce(28350m<milligram>))
        Assert.Equal(1.0000013889122517647287585547m<pound>,milligramsToPounds(453593m<milligram>))
        Assert.Equal(1.0000013889122517647287585547m<stone>,milligramsToStone(6350302m<milligram>))
        Assert.Equal(1.0000013889122517647287585547m<quarter_wt_shrt>,milligramsToQuartersShrt(11339825m<milligram>))
        Assert.Equal(1.0000013889122517647287585548m<quarter_wt_lng>,milligramsToQuartersLng(12700604m<milligram>))
        Assert.Equal(1.0000013889122517647287585547m<hundredweight_shrt>,milligramsToHundredweightsShrt(45359300m<milligram>))
        Assert.Equal(1.0000013889122517647287585548m<hundredweight_lng>,milligramsToHundredweightsLng(50802416m<milligram>))
        Assert.Equal(1.0000013889122517647287585548m<ton_shrt>,milligramsToTonsShrt(907186000m<milligram>))
        Assert.Equal(1.0000013889122517647287585548m<ton_lng>,milligramsToTonsLng(1016048320m<milligram>))

    [<Fact>]
    let ``Grams(g) to Other`` () =
        Assert.Equal(1000m<milligram>,gramsToMilligrams(1m<gram>))
        Assert.Equal(1m<kilogram>,gramsToKilograms(1000m<gram>))
        Assert.Equal(1m<ton_metric>,gramsToTonsMetric(1000000m<gram>))
        Assert.Equal(15.432358352941430650608166000m<grain>,gramsToGrains(1m<gram>))
        Assert.Equal(1.001780519368083726805192960m<drachm>,gramsToDrachms(1.775m<gram>))
        Assert.Equal(1.0000520952325542865723248384m<ounce>,gramsToOunces(28.351m<gram>))
        Assert.Equal(1.0000013889122517647287585547m<pound>,gramsToPounds(453.593m<gram>))
        Assert.Equal(1.0000010739661629291893575219m<stone>,gramsToStones(6350.30m<gram>))
        Assert.Equal(1.000008002780117311056180244m<quarter_wt_shrt>,gramsToQuartersShrt(11339.9m<gram>))
        Assert.Equal(1.000001073966162929189357522m<quarter_wt_lng>,gramsToQuartersLng(12700.6m<gram>))
        Assert.Equal(1.0000013889122517647287585547m<hundredweight_shrt>,gramsToHundredweightsShrt(45359.3m<gram>))
        Assert.Equal(1.0000010739661629291893575219m<hundredweight_lng>,gramsToHundredweightsLng(50802.4m<gram>))
        Assert.Equal(1.0000002866009408403408549399m<ton_shrt>,gramsToTonsShrt(907185m<gram>))
        Assert.Equal(1.000544355969404234656139136m<ton_lng>,gramsToTonsLng(1016600m<gram>))
    

    [<Fact>]
    let ``Kilograms(g) to Other`` () =
        Assert.Equal(1000000m<milligram>,kilogramsToMilligrams(1m<kilogram>))
        Assert.Equal(1000m<gram>,kilogramsToGrams(1m<kilogram>))
        Assert.Equal(15432.358352941430650608166094m<grain>,kilogramsToGrains(1m<kilogram>))
        Assert.Equal(564.38339119328660665081293146m<drachm>,kilogramsToDrachms(1m<kilogram>))
        Assert.Equal(35.273961949580412915675808216m<ounce>,kilogramsToOunces(1m<kilogram>))
        Assert.Equal(1m<pound>,kilogramsToPounds(0.45359237m<kilogram>))
        Assert.Equal(1m<stone>,kilogramsToStones(6.35029318m<kilogram>))
        Assert.Equal(1m<quarter_wt_shrt>,kilogramsToQuartersShrt(11.33980925m<kilogram>))
        Assert.Equal(1m<quarter_wt_lng>,kilogramsToQuartersLng(12.70058636m<kilogram>))
        Assert.Equal(1m<hundredweight_shrt>,kilogramsToHundredweightsShrt(45.359237m<kilogram>))
        Assert.Equal(1m<hundredweight_lng>,kilogramsToHundredweightsLng(50.80234544m<kilogram>))
        Assert.Equal(1m<ton_shrt>,kilogramsToTonsShrt(907.18474m<kilogram>))
        Assert.Equal(1m<ton_lng>,kilogramsToTonsLng(1016.0469088m<kilogram>))
        Assert.Equal(1m<ton_metric>,kilogramsToTonsMetric(1000m<kilogram>))

    [<Fact>]
    let ``Grains(gr) to Other`` () =
        Assert.Equal(1.6199727500000000000000000000m<gram>,grainsToGrams(25m<grain>))
        Assert.Equal(64.79891m<milligram>,grainsToMilligrams(1m<grain>))
        Assert.Equal(0.45359237m<kilogram>,grainsToKilograms(7000m<grain>))
        Assert.Equal(0.09998471813000000000000000m<ton_metric>,grainsToMetricTons(1543000m<grain>))
        Assert.Equal(1.0000018285714285714285714176m<drachm>,grainsToDrachms(27.3438m<grain>))
        Assert.Equal(1m<ounce>,grainsToOunces(437.5m<grain>))
        Assert.Equal(1m<pound>,grainsToPounds(7000m<grain>))
        Assert.Equal(1m<stone>,grainsToStones(98000m<grain>))
        Assert.Equal(1m<quarter_wt_lng>,grainsToQuartersLng(196000m<grain>))
        Assert.Equal(1m<quarter_wt_shrt>,grainsToQuartersShrt(175000m<grain>))
        Assert.Equal(1m<hundredweight_lng>,grainsToHundredweightsLng(784000m<grain>))
        Assert.Equal(1.0000014285714285714285714286m<hundredweight_shrt>,grainsToHundredweightsShrt(700001m<grain>))
        Assert.Equal(1.00000050000000000000000000m<ton_shrt>,grainsToTonsShrt(14000007m<grain>))
        Assert.Equal(1.0000004464285714285714285715m<ton_lng>,grainsToTonsLng(15680007m<grain>))
        Assert.Equal(4.1988980891989999999999999999m<ton_metric>,grainsToTonsMetric(64798900m<grain>))

    [<Fact>]
    let ``Drachm to Other`` () = 
        Assert.Equal(1771.8451953125m<milligram>,drachmToMilligrams(1m<drachm>))
        Assert.Equal(1.7718451953125000m<gram>,drachmsToGrams(1m<drachm>))
        Assert.Equal(0.45359237m<kilogram>,drachmsToKilograms(256m<drachm>))
        Assert.Equal(0.45359237m<ton_metric>,drachmsToTonsMetric(256000m<drachm>))
        Assert.Equal(27.34375m<grain>,drachmsToGrains(1m<drachm>))
        Assert.Equal(1m<ounce>,drachmsToOunces(16m<drachm>))
        Assert.Equal(1m<pound>,drachmsToPounds(256m<drachm>))
        Assert.Equal(1m<stone>,drachmsToStones(3584m<drachm>))
        Assert.Equal(1m<quarter_wt_shrt>,drachmsToQuartersShrt(6400m<drachm>))
        Assert.Equal(1m<quarter_wt_lng>,drachmsToQuartersLng(7168m<drachm>))
        Assert.Equal(1m<hundredweight_shrt>,drachmsToHundredweightsShrt(25600m<drachm>))
        Assert.Equal(1m<hundredweight_lng>,drachmsToHundredweightsLng(28672m<drachm>))
        Assert.Equal(1m<ton_shrt>,drachmsToTonsShrt(512000m<drachm>))
        Assert.Equal(1m<ton_lng>,drachmsToTonsLng(573440m<drachm>))

    [<Fact>]
    let ``Ounces to Other`` () =
        Assert.Equal(28349.523125m<milligram>,ouncesToMilligrams(1m<ounce>))
        Assert.Equal(28.349523125000m<gram>,ouncesToGrams(1m<ounce>))
        Assert.Equal(0.45359237m<kilogram>,ouncesToKilograms(16m<ounce>))
        Assert.Equal(0.45359237m<ton_metric>,ouncesToTonsMetric(16000m<ounce>))
        Assert.Equal(437.5m<grain>,ouncesToGrains(1m<ounce>))
        Assert.Equal(16m<drachm>,ouncesToDrachms(1m<ounce>))
        Assert.Equal(1m<pound>,ouncesToPounds(16m<ounce>))
        Assert.Equal(1m<stone>,ouncesToStones(224m<ounce>))
        Assert.Equal(1m<quarter_wt_shrt>,ouncesToQuartersShrt(400m<ounce>))
        Assert.Equal(1m<quarter_wt_lng>,ouncesToQuartersLng(448m<ounce>))
        Assert.Equal(1m<hundredweight_shrt>,ouncesToHundredweightsShrt(1600m<ounce>))
        Assert.Equal(1m<hundredweight_lng>,ouncesToHundredweightsLng(1792m<ounce>))
        Assert.Equal(1m<ton_shrt>,ouncesToTonsShrt(32000m<ounce>))
        Assert.Equal(1m<ton_lng>,ouncesToTonsLng(35840m<ounce>))

    [<Fact>]
    let ``Pounds(lb) to Other`` () =
        Assert.Equal(453592.37000000m<milligram>,poundsToMilligrams(1m<pound>))
        Assert.Equal(453.59237000m<gram>,poundsToGrams(1m<pound>))
        Assert.Equal(0.45359237m<kilogram>,poundsToKilograms(1m<pound>))
        Assert.Equal(0.999989738902m<ton_metric>,poundsToTonsMetric(2204.6m<pound>))
        Assert.Equal(7000m<grain>,poundsToGrains(1m<pound>))
        Assert.Equal(256m<drachm>,poundsToDrachms(1m<pound>))
        Assert.Equal(16m<ounce>,poundsToOunces(1m<pound>))
        Assert.Equal(1m<stone>,poundsToStones(14m<pound>))
        Assert.Equal(1m<quarter_wt_shrt>,poundsToQuartersShrt(25m<pound>))
        Assert.Equal(1m<quarter_wt_lng>,poundsToQuartersLng(28m<pound>))
        Assert.Equal(1m<hundredweight_shrt>,poundsToHundredweightsShrt(100m<pound>))
        Assert.Equal(1m<hundredweight_lng>,poundsToHundredweightsLng(112m<pound>))
        Assert.Equal(1m<ton_shrt>,poundsToTonsShrt(2000m<pound>))
        Assert.Equal(1m<ton_lng>,poundsToTonsLng(2240m<pound>))

    [<Fact>]
    let ``Stones(14lb) to Other`` () =
        Assert.Equal(6350293.18m<milligram>,stonesToMilligrams(1m<stone>))
        Assert.Equal(6350.29318m<gram>,stonesToGrams(1m<stone>))
        Assert.Equal(6.35029318m<kilogram>,stonesToKilograms(1m<stone>))
        Assert.Equal(1.0000000036973331m<ton_metric>,stonesToTonsMetric(157.473045m<stone>))
        Assert.Equal(98000m<grain>,stonesToGrains(1m<stone>))
        Assert.Equal(3584m<drachm>,stonesToDrachms(1m<stone>))
        Assert.Equal(224m<ounce>,stonesToOunces(1m<stone>))
        Assert.Equal(14m<pound>,stonesToPounds(1m<stone>))
        Assert.Equal(1.12m<quarter_wt_shrt>,stonesToQuartersShrt(2m<stone>))
        Assert.Equal(1m<quarter_wt_lng>,stonesToQuartersLng(2m<stone>))
        Assert.Equal(1.12m<hundredweight_shrt>,stonesToHundredweightsShrt(8m<stone>))
        Assert.Equal(1m<hundredweight_lng>,stonesToHundredweightsLng(8m<stone>))
        Assert.Equal(1.001m<ton_shrt>,stonesToTonsShrt(143m<stone>))
        Assert.Equal(1m<ton_lng>,stonesToTonsLng(160m<stone>))

    [<Fact>]
    let ``Quarters Short(25lb) to Other`` () =
        Assert.Equal(11339809.25m<milligram>,quartersShrtToMilligrams(1m<quarter_wt_shrt>))
        Assert.Equal(11339.80925000m<gram>,quartersShrtToGrams(1m<quarter_wt_shrt>))
        Assert.Equal(11.33980925m<kilogram>,quartersShrtToKilograms(1m<quarter_wt_shrt>))
        Assert.Equal(1.133980925m<ton_metric>,quartersShrtToTonsMetric(100m<quarter_wt_shrt>))
        Assert.Equal(175000m<grain>,quartersShrtToGrains(1m<quarter_wt_shrt>))
        Assert.Equal(6400m<drachm>,quartersShrtToDrachms(1m<quarter_wt_shrt>))
        Assert.Equal(400m<ounce>,quartersShrtToOunces(1m<quarter_wt_shrt>))
        Assert.Equal(25m<pound>,quartersShrtToPounds(1m<quarter_wt_shrt>))
        Assert.Equal(1.7857142857142857142857142856m<stone>,quartersShrtToStones(1m<quarter_wt_shrt>))
        Assert.Equal(0.8928571428571428571428571428m<quarter_wt_lng>,quartersShrtToQuartersLng(1m<quarter_wt_shrt>))
        Assert.Equal(1m<hundredweight_shrt>,quartersShrtToHundredweightsShrt(4m<quarter_wt_shrt>))
        Assert.Equal(1.1160714285714285714285714286m<hundredweight_lng>,quartersShrtToHundredweightsLng(5m<quarter_wt_shrt>))
        Assert.Equal(1m<ton_shrt>,quartersShrtToTonsShrt(80m<quarter_wt_shrt>))
        Assert.Equal(1.1160714285714285714285714286m<ton_lng>, quartersShrtToTonsLng(100m<quarter_wt_shrt>))

    [<Fact>]
    let ``Quarters Long(28lb) to Other`` () =
        Assert.Equal(12700586.36m<milligram>,quartersLngToMilligrams(1m<quarter_wt_lng>))
        Assert.Equal(12700.58636000m<gram>,quartersLngToGrams(1m<quarter_wt_lng>))
        Assert.Equal(12.70058636m<kilogram>,quartersLngToKilograms(1m<quarter_wt_lng>))
        Assert.Equal(27.9999999765194632m<ton_metric>,quartersLngToTonsMetric(2204.62262m<quarter_wt_lng>))
        Assert.Equal(196000m<grain>,quartersLngToGrains(1m<quarter_wt_lng>))
        Assert.Equal(7168m<drachm>,quartersLngToDrachms(1m<quarter_wt_lng>))
        Assert.Equal(448m<ounce>,quartersLngToOunces(1m<quarter_wt_lng>))
        Assert.Equal(28m<pound>,quartersLngToPounds(1m<quarter_wt_lng>))
        Assert.Equal(2m<stone>,quartersLngToStones(1m<quarter_wt_lng>))
        Assert.Equal(1.1200m<quarter_wt_shrt>,quartersLngToQuartersShrt(1m<quarter_wt_lng>))
        Assert.Equal(1.12m<hundredweight_shrt>,quartersLngToHundredweightsShrt(4m<quarter_wt_lng>))
        Assert.Equal(1m<hundredweight_lng>,quartersLngToHundredweightsLng(4m<quarter_wt_lng>))
        Assert.Equal(0.140m<ton_shrt>,quartersLngToTonsShrt(10m<quarter_wt_lng>))
        Assert.Equal(1m<ton_lng>,quartersLngToTonsLng(80m<quarter_wt_lng>))

    [<Fact>]
    let ``Hundredweights Shrt(100lb) to Other`` () =
        Assert.Equal(45359237m<milligram>,hundredweightsShrtToMilligrams(1m<hundredweight_shrt>))
        Assert.Equal(45359.23700000m<gram>,hundredweightsShrtToGrams(1m<hundredweight_shrt>))
        Assert.Equal(45.35923700m<kilogram>,hundredweightsShrtToKilograms(1m<hundredweight_shrt>))
        Assert.Equal(1.043262451m<ton_metric>,hundredweightsShrtToTonsMetric(23m<hundredweight_shrt>))
        Assert.Equal(700000m<grain>,hundredweightsShrtToGrains(1m<hundredweight_shrt>))
        Assert.Equal(25600m<drachm>,hundredweightsShrtToDrachms(1m<hundredweight_shrt>))
        Assert.Equal(1600m<ounce>,hundredweightsShrtToOunces(1m<hundredweight_shrt>))
        Assert.Equal(100m<pound>,hundredweightsShrtToPounds(1m<hundredweight_shrt>))
        Assert.Equal(7.1428571428571428571428571432m<stone>,hundredweightsShrtToStones(1m<hundredweight_shrt>))
        Assert.Equal(4m<quarter_wt_shrt>,hundredweightsShrtToQuartersShrt(1m<hundredweight_shrt>))
        Assert.Equal(3.5714285714285714285714285716m<quarter_wt_lng>,hundredweightsShrtToQuartersLng(1m<hundredweight_shrt>))
        Assert.Equal(1m<hundredweight_lng>,hundredweightsShrtToHundredweightsLng(1.12m<hundredweight_shrt>))  
        Assert.Equal(1m<ton_shrt>,hundredweightsShrtToTonsShrt(20m<hundredweight_shrt>))
        Assert.Equal(0.9821428571428571428571428572m<ton_lng>,hundredweightsShrtToTonsLng(22m<hundredweight_shrt>))

    [<Fact>]
    let ``Hundredweights Long(112lb) to Other`` () =
        Assert.Equal(50802345.44m<milligram>,hundredweightsLngToMilligrams(1m<hundredweight_lng>))
        Assert.Equal(50802.34544000m<gram>,hundredweightsLngToGrams(1m<hundredweight_lng>))
        Assert.Equal(50.80234544m<kilogram>,hundredweightsLngToKilograms(1m<hundredweight_lng>))
        Assert.Equal(1.1196836934976m<ton_metric>,hundredweightsLngToTonsMetric(22.04m<hundredweight_lng>))
        Assert.Equal(784000m<grain>,hundredweightsLngToGrains(1m<hundredweight_lng>))
        Assert.Equal(28672m<drachm>,hundredweightsLngToDrachms(1m<hundredweight_lng>))
        Assert.Equal(1792m<ounce>,hundredweightsLngToOunces(1m<hundredweight_lng>))
        Assert.Equal(112m<pound>,hundredweightsLngToPounds(1m<hundredweight_lng>))
        Assert.Equal(8m<stone>,hundredweightsLngToStones(1m<hundredweight_lng>))
        Assert.Equal(4.48m<quarter_wt_shrt>, hundredweightsLngToQuartersShrt(1m<hundredweight_lng>))
        Assert.Equal(4m<quarter_wt_lng>,hundredweightsLngToQuartersLng(1m<hundredweight_lng>))
        Assert.Equal(1.12m<hundredweight_shrt>,hundredweightsLngToHundredweightsShrt(1m<hundredweight_lng>))
        Assert.Equal(1.008m<ton_shrt>,hundredweightsLngToTonsShrt(18m<hundredweight_lng>))
        Assert.Equal(1m<ton_lng>,hundredweightsLngToTonsLng(20m<hundredweight_lng>))

    [<Fact>]
    let ``Tons Short(2000lb) to Other`` () =
        Assert.Equal(907184740m<milligram>,tonsShrtToMilligrams(1m<ton_shrt>))
        Assert.Equal(907184.74000000m<gram>,tonsShrtToGrams(1m<ton_shrt>))
        Assert.Equal(907.18474000m<kilogram>,tonsShrtToKilograms(1m<ton_shrt>))
        Assert.Equal(0.90718474m<ton_metric>,tonsShrtToTonsMetric(1m<ton_shrt>))
        Assert.Equal(14000000m<grain>,tonsShrtToGrains(1m<ton_shrt>))
        Assert.Equal(512000m<drachm>,tonsShrtToDrachms(1m<ton_shrt>))
        Assert.Equal(32000m<ounce>,tonsShrtToOunces(1m<ton_shrt>))
        Assert.Equal(2000m<pound>,tonsShrtToPounds(1m<ton_shrt>))
        Assert.Equal(142.85714285714285714285714286m<stone>,tonsShrtToStones(1m<ton_shrt>))
        Assert.Equal(80m<quarter_wt_shrt>,tonsShrtToQuartersShrt(1m<ton_shrt>))
        Assert.Equal(71.428571428571428571428571428m<quarter_wt_lng>,tonsShrtToQuartersLng(1m<ton_shrt>))
        Assert.Equal(20m<hundredweight_shrt>,tonsShrtToHundredweightsShrt(1m<ton_shrt>))
        Assert.Equal(17.857142857142857142857142857m<hundredweight_lng>,tonsShrtToHundredweightsLng(1m<ton_shrt>))
        Assert.Equal(0.8928571428571428571428571428m<ton_lng>,tonsShrtToTonsLng(1m<ton_shrt>))

    [<Fact>]
    let ``Tons Long(2240lb) to Other`` () =
        Assert.Equal(1016046908.8m<milligram>,tonsLngToMilligrams(1m<ton_lng>))
        Assert.Equal(1016046.90880000m<gram>,tonsLngToGrams(1m<ton_lng>))
        Assert.Equal(1016.04690880m<kilogram>,tonsLngToKilograms(1m<ton_lng>))
        Assert.Equal(1.01604690880m<ton_metric>,tonsLngToTonsMetric(1m<ton_lng>))
        Assert.Equal(15680000m<grain>,tonsLngToGrains(1m<ton_lng>))
        Assert.Equal(573440m<drachm>,tonsLngToDrachms(1m<ton_lng>))
        Assert.Equal(35840m<ounce>,tonsLngToOunces(1m<ton_lng>))
        Assert.Equal(2240m<pound>,tonsLngToPounds(1m<ton_lng>))
        Assert.Equal(160m<stone>,tonsLngToStones(1m<ton_lng>))
        Assert.Equal(89.6m<quarter_wt_shrt>,tonsLngToQuartersShrt(1m<ton_lng>))
        Assert.Equal(22.4m<hundredweight_shrt>,tonsLngToHundredweightsShrt(1m<ton_lng>))
        Assert.Equal(20m<hundredweight_lng>,tonsLngToHundredweightsLng(1m<ton_lng>))
        Assert.Equal(80m<quarter_wt_lng>,tonsLngToQuartersLng(1m<ton_lng>))
        Assert.Equal(1.12m<ton_shrt>,tonsLngToTonsShrt(1m<ton_lng>))


