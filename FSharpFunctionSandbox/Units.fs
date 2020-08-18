module Units

open Microsoft.FSharp.Data.UnitSystems.SI

module Imperial =
    module UnitNames =
        //Length
        [<Measure>] type thou
        [<Measure>] type inch //25.4mm international
        [<Measure>] type foot //12 in
        [<Measure>] type yard //.9144m international, 3ft
        [<Measure>] type chain //22 yards
        [<Measure>] type football_field //100 yard
        [<Measure>] type furlong //10 chains
        [<Measure>] type mile //8 furlongs
        [<Measure>] type league // 3 miles

        //Volume
        [<Measure>] type minim_us
        [<Measure>] type fluidDram_us //60 min
        [<Measure>] type teaspoon // 80 min
        [<Measure>] type tablespoon // 3 tsp
        [<Measure>] type fluidOunce_us //2 tbsp, 29.5735295625ml
        [<Measure>] type shot // 3 tbsp
        [<Measure>] type gill_us // 4 fl oz
        [<Measure>] type cup // 8 fl oz
        [<Measure>] type pint_us // 2 cups
        [<Measure>] type quart_us // 2 pint
        [<Measure>] type pottle // 2 qt
        [<Measure>] type gallon_us // 4 qt, 231 in^3
        [<Measure>] type barrel //31.5 gal
        [<Measure>] type hogshead //63 gal
        [<Measure>] type cubicInch = inch ^ 3
        [<Measure>] type minim_uk // 1/60 fl drachm
        [<Measure>] type fluidDrachm_uk // 1/8 fl oz
        [<Measure>] type fluidOunce_uk // 1/5 gill
        [<Measure>] type gill_uk // 1/4 pt, 4.804 US fl oz
        [<Measure>] type pint_uk // 1/2 qt
        [<Measure>] type quart_uk // 1/4 gal, 1.201 us qt
        [<Measure>] type gallon_uk//4.5609 l (1985), 1.201 us gal
        [<Measure>] type peck// 2 gal, 24.02 us gal
        [<Measure>] type bushel_uk // 8 gal
        [<Measure>] type quarter_vl_uk

        //Weight
        [<Measure>] type grain
        [<Measure>] type drachm
        [<Measure>] type ounce
        [<Measure>] type pound // 0.45359237kg international
        [<Measure>] type stone

        //The difference between lng and shrt is that shrt is
        //hundredweight = 100 vs 112
        [<Measure>] type quarter_wt_lng
        [<Measure>] type quarter_wt_shrt
        [<Measure>] type hundredweight_lng
        [<Measure>] type hundredweight_shrt
        [<Measure>] type ton_lng
        [<Measure>] type ton_shrt

        [<Measure>] type ton_metric
        [<Measure>] type tonne = ton_metric

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