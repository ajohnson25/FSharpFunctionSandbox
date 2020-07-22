module Units

open Microsoft.FSharp.Data.UnitSystems.SI

module Imperial =
    module UnitNames =
        //Length
        [<Measure>] type thou
        [<Measure>] type inch
        [<Measure>] type foot
        [<Measure>] type yard //.9144m
        [<Measure>] type chain
        [<Measure>] type furlong
        [<Measure>] type mile
        [<Measure>] type league

        //Volume
        [<Measure>] type minim_US
        [<Measure>] type fluidDram_us //60 min
        [<Measure>] type teaspoon_us // 80 min
        [<Measure>] type tablespoon_us // 3 tsp
        [<Measure>] type fluidOunce_us //2 tbsp, 29.5735295625ml
        [<Measure>] type shot_us // 3 tbsp
        [<Measure>] type gill_us // 4 fl oz
        [<Measure>] type cup_us // 8 fl oz
        [<Measure>] type pint_us // 2 cups
        [<Measure>] type quart_us // 2 pint
        [<Measure>] type pottle_us // 2 qt
        [<Measure>] type gallon_us // 4 qt
        [<Measure>] type barrel //31.5 gal
        [<Measure>] type hogshead //63 gal
        
        [<Measure>] type minim_uk // 1/60 fl drachm
        [<Measure>] type fluidDrachm_uk // 1/8 fl oz
        [<Measure>] type fluidOunce_uk // 1/5 gill
        [<Measure>] type gill_uk // 1/4 pt, 4.804 US fl oz
        [<Measure>] type pint_uk // 1/2 qt
        [<Measure>] type quart_uk // 1/4 gal, 1.201 us qt
        [<Measure>] type gallon_uk//4.5609 l (1985), 1.201 us gal
        [<Measure>] type peck_uk// 2 gal, 24.02 us gal
        [<Measure>] type bushel_uk // 8 gal
        [<Measure>] type quarter_vl_uk

        //Weight
        [<Measure>] type grain
        [<Measure>] type drachm
        [<Measure>] type ounce
        [<Measure>] type pound // 0.45359237kg
        [<Measure>] type stone
        [<Measure>] type quarter_wt
        [<Measure>] type hundredweight
        [<Measure>] type ton

//User friendly representation of the derived SI units
module SI =
    module UnitNames =
        //Weight
        [<Measure>] type gram
        [<Measure>] type milligram
        [<Measure>] type kilogram = UnitNames.kilogram

        //Length
        [<Measure>] type centimeter
        [<Measure>] type millimeter
        [<Measure>] type meter = UnitNames.meter

        //Volume
        [<Measure>] type milliliter = millimeter^3
        [<Measure>] type liter = meter^3