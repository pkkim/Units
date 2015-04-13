module USVolume
    ( teaspoon
    , tablespoon
    , fluidOunce
    , cup
    , pint
    , quart
    , gallon
    , barrel
    , oilBarrel
    ) where

{-| US units of volume.

@docs teaspoon, tablespoon, fluidOunce, cup, pint, quart, gallon, barrel,
oilBarrel
-}

import Ratio
import SI (..)
import Units (..)

-- All these units are defined in terms of SI units.
cubicMeter : Unit
cubicMeter = meter `pow` (Ratio.fromInt 3)
liter : Unit
liter = scale 0.001 cubicMeter
ml : Unit
ml = scale 0.001 liter

{-| One teaspoon. -}
teaspoon : Unit
teaspoon = scale 4.92892159375 ml
{-| One tablespoon. -}
tablespoon : Unit
tablespoon = scale 3 teaspoon
{-| One US fluid ounce. -}
fluidOunce : Unit
fluidOunce = scale 2 tablespoon
{-| One US cup, or eight US fluid ounces. -}
cup : Unit
cup = scale 8 fluidOunce
{-| One US pint, or two US cups. -}
pint : Unit
pint = scale 2 cup
{-| One US quart, or two US pints. -}
quart : Unit
quart = scale 2 pint
{-| One US gallon, or four US quarts. -}
gallon : Unit
gallon = scale 4 quart
{-| One barrel, or 31.5 US gallons. -}
barrel : Unit
barrel = scale 31.5 gallon
{-| One petroleum barrel, or 42 US gallons. -}
oilBarrel : Unit
oilBarrel = scale 42 gallon
