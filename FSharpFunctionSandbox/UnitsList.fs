namespace FSharpFunctions

module UnitsList =

    let volumeList = ["minimsUs";
    "fluidDramsUs";
    "teaspoonsUs";
    "tablespoonsUs";
    "fluidOuncesUs";
    "shotsUs";
    "gillsUs";
    "cupsUs";
    "pintsUs";
    "quartsUs";
    "pottlesUs";
    "gallonsUs";
    "barrelsUs";
    "hogsheadsUs";
    "cubicInches";
    "minimsUk";
    "fluidDrachmsUk";
    "fluidOuncesUk";
    "gillsUk";
    "pintsUk";
    "quartsUk";
    "gallonsUk";
    "pecksUk";
    "bushelsUk";
    "quartersUk";
    "milliliters";
    "liters"]

    let volumeListExcept value = List.filter (fun i -> i <> value) volumeList

    let weightList = ["grains";
    "drachms";
    "ounces";
    "pounds";
    "stones";
    "quartersLng";
    "quartersShrt";
    "hundredweightsLng";
    "hundredweightsShrt";
    "tonsLng";
    "tonsShrt";
    "tonsMetric";
    "milligrams";
    "grams";
    "kilograms"]

    let lengthList = ["thou";
    "inch";
    "foot";
    "yard";
    "millimeter";
    "centimeter";
    "meter"]