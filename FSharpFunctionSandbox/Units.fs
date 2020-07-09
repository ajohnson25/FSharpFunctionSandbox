module Units

open Microsoft.FSharp.Data.UnitSystems.SI

module Imperial =
    module Name =
        //Length
        [<Measure>] type thou
        [<Measure>] type inch
        [<Measure>] type foot
        [<Measure>] type yard
        [<Measure>] type chain
        [<Measure>] type furlong
        [<Measure>] type mile
        [<Measure>] type league

        //Volume
        [<Measure>] type fluidOunce
        [<Measure>] type gill
        [<Measure>] type pint
        [<Measure>] type quart
        [<Measure>] type gallon
        [<Measure>] type peck
        [<Measure>] type bushel
        [<Measure>] type quarter_vl

        //Weight
        [<Measure>] type grain
        [<Measure>] type drachm
        [<Measure>] type ounce
        [<Measure>] type pound
        [<Measure>] type stone
        [<Measure>] type quarter_wt
        [<Measure>] type hundredweight
        [<Measure>] type ton
        [<Measure>] type slug

//User friendly representation of the derived SI units
module SIUF =
    module Name =
        //Weight
        [<Measure>] type gram
        [<Measure>] type milligram

        //Length
        [<Measure>] type centimeter
        [<Measure>] type millimeter
