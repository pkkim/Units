module ImperialLength
    ( thou
    , inch
    , foot
    , yard
    , chain
    , furlong
    , mile
    , league
    , fathom
    , cable
    , nMile
    , link
    , rod
    , chain
    ) where

{-| Imperial units of length.

@docs thou, inch, foot, yard, chain, furlong, mile, league, fathom, cable,
nauticalMile, link, rod, chain
-}

import Units (..)
import SI (..)

{-| One thousandth of an inch, or 0.0254 millimeters. -}
thou : Unit
thou = scale 0.0000254 meter
{-| Unit equal to 0.0254 meters. -}
inch : Unit
inch = scale 0.0254 meter
{-| Unit equal to 0.3048 meters. -}
foot : Unit
foot = scale 0.3048 meter
{-| Three feet, or 0.9144 meters. -}
yard : Unit
yard = scale 0.9144 meter
{-| 22 yards, or 20.1168 meters. -}
chain : Unit
chain = scale 20.1168 meter
{-| Ten chains, or 201.168 meters. -}
furlong : Unit
furlong = scale 201.168 meter
{-| 5280 feet, or 1609.344 meters. -}
mile : Unit
mile = scale 1609.344 meter
{-| Three miles in land usage, or 4828.032 meters. -}
league : Unit
league = scale 4828.032 meter
{-| Six feet or 1.8288 meters. -}
fathom : Unit
fathom = scale 1.8288 meter
{-| One tenth of a nautical mile or 185.3184 meters. -}
cable : Unit
cable = scale 185.3184 meter
{-| 1,852 meters, or about 6,076 feet. -}
nMile : Unit
nMile = scale 1853.184 meter
{-| Also called Gunter's link. Exactly 66/100 of a foot, or 0.201168 meters. -}
link : Unit
link = scale 0.201168 meter
{-| Five and a half yards, or 5.0292 meters. -}
rod : Unit
rod = scale 5.0292 meter
