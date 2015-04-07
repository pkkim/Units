module Units
  ( Unit
  , convert
  , mul
  , scale
  , inv
  , per
  , pow
  , affineUnit
  , unity
  , baseLength
  , baseTime
  , baseMass
  , baseTemp
  , baseAmount
  , baseCurrent
  , baseLumInt
  ) where

{-| A module providing unit conversion capabilities. Units themselves may be
defined in other modules.

# Types
@docs Unit

# Constructors
@docs unity, baseLength, baseTime, baseMass, baseTemp, baseAmount, baseCurrent,
baseLumInt

# Unit operators
@docs mul, scale, inv, per, pow, affineUnit

# Conversion
@docs convert

Examples:

Converting meters to feet, and vice versa:

    meter : Unit
    meter = baseLength
    foot : Unit
    foot = scale 0.3048 meter
    result = convert 6 foot meter
        (result == Just 1.8288)

Converting Kelvin to Fahrenheit:

    kelvin = baseTemp
    fahrenheit = affineUnit 255.372 (scale (5/9) kelvin)
    result = convert 300 kelvin fahrenheit
        (result == Just 80.33)

Units whose zero does not match the physical zero of the unit cannot be
multiplied or divided. They can be scaled, however:

    kelvin = baseTemp
    celsius = affineUnit 273.15 kelvin
    reaumur = scale (100/80) celsius
    result = convert (-30) reaumur celsius
        (result == Just -37.5)
    fahrenheit = affineUnit 255.372 (scale (5/9) kelvin)
    result2 = convert (-30) reaumur fahrenheit
        (result2 == Just -35.5)

More elaborate conversions:

    meter = baseLength
    cubicMeter = meter `pow` (fromInt 3)
    mile = scale 1609.34 meter
    gallon = scale 0.00378541178 cubicMeter
    mpg = mile `per` gallon
    liter = scale 0.001 cubicMeter
    kilometer = scale 1000 meter
    kmpl = kilometer `per` liter
    result = convert 30 mpg kmpl
        (result ~== Just 12.754)

Invalid conversion:

    meter = baseLength
    cubicMeter = meter `pow` (fromInt 3)
    mile = scale 1609.34 meter
    result = convert 5 mile cubicMeter
        (result == Nothing)
-}

import Ratio (..)
import Ratio

{-| Represents a single unit of a physical quantity, such as square meters,
moles per gallon, etc. Units must be either linear or affine. -}
type alias Unit = {
    len : Rational
  , time : Rational
  , mass : Rational
  , temp : Rational
  , amount : Rational
  , current : Rational
  , lumInt : Rational
  , prefix : Float
  , zero : Float }

{-| Constructor for the unitless quantity of magnitude one. It has the
property: for any unit x, x `mul` unity == x.
-}
unity : Unit
unity = {
    len = fromInt 0
  , time = fromInt 0
  , mass = fromInt 0
  , temp = fromInt 0
  , amount = fromInt 0
  , current = fromInt 0
  , lumInt = fromInt 0
  , prefix = 1
  , zero = 0 }

{-| Constructor for the length quantity of magnitude one. -}
baseLength : Unit
baseLength = { unity | len <- fromInt 1 }

{-| Constructor for the time quantity of magnitude one. -}
baseTime : Unit
baseTime = { unity | time <- fromInt 1 }

{-| Constructor for the mass quantity of magnitude one. -}
baseMass : Unit
baseMass = { unity | mass <- fromInt 1 }

{-| Constructor for the temperature quantity of magnitude one. -}
baseTemp : Unit
baseTemp = { unity | temp <- fromInt 1 }

{-| Constructor for the amount quantity of magnitude one. -}
baseAmount : Unit
baseAmount = { unity | amount <- fromInt 1 }

{-| Constructor for the current quantity of magnitude one. -}
baseCurrent : Unit
baseCurrent = { unity | current <- fromInt 1 }

{-| Constructor for the luminous intensity quantity of magnitude one. -}
baseLumInt : Unit
baseLumInt = { unity | lumInt <- fromInt 1 }

{-| Scales a unit by a factor.

    meter = baseLength
    kilometer = scale 1000 meter
-}
scale : number -> Unit -> Unit
scale scalar unit = { unit | prefix <- unit.prefix * scalar }

{-| Multiplies two units.

    foot = baseLength
    kilogram = baseMass
    weirdEnergyUnit = foot `mul` kilogram
-}
mul : Unit -> Unit -> Unit
mul x y = {
    len = x.len `add` y.len
  , time = x.time `add` y.time
  , mass = x.mass `add` y.mass
  , temp = x.temp `add` y.temp
  , amount = x.amount `add` y.amount
  , current = x.current `add` y.current
  , lumInt = x.lumInt `add` y.lumInt
  , prefix = x.prefix * y.prefix
  , zero = 0 }

{-| Inverts a unit.

    second = baseTime
    hertz = inv second
-}
inv : Unit -> Unit
inv unit =
  { len = Ratio.negate (unit.len)
  , time = Ratio.negate (unit.time)
  , mass = Ratio.negate (unit.mass)
  , temp = Ratio.negate (unit.temp)
  , amount = Ratio.negate (unit.amount)
  , current = Ratio.negate (unit.current)
  , lumInt = Ratio.negate (unit.lumInt)
  , prefix = 1 / unit.prefix
  , zero = 0 }

{-| Divides the first argument by the second argument.

    meter = baseLength
    liter = scale 0.001 (meter `mul` meter `mul` meter)
    kilometer = scale 1000 meter
    kmph = kilometer `per` liter
-}
per : Unit -> Unit -> Unit
per numerator denominator = numerator `mul` (inv denominator)

{-| Raises a unit to a rational power.

    massVariance = scale (pow baseMass (Ratio.fromInt 2)) (scalar : Float)
    massStDev = pow massVariance (1 `Ratio.over` 2)
-}
pow : Unit -> Rational -> Unit
pow unit power = 
  { len = unit.len `multiply` power
  , time = unit.time `multiply` power
  , mass = unit.mass `multiply` power
  , temp = unit.temp `multiply` power
  , amount = unit.amount `multiply` power
  , current = unit.current `multiply` power
  , lumInt = unit.lumInt `multiply` power
  , prefix = unit.prefix ^ Ratio.toFloat power
  , zero = unit.zero ^ Ratio.toFloat power }

{-| Makes unit whose zero is not physical zero. Where X is the second argument,
the zero of the new unit is X base units above the physical zero.

    kelvin = baseTemp
    celsius = affineUnit 255 kelvin
    fahrenheit = affineUnit 273.15 (scale (5/9) celsius)
-}
affineUnit : Float -> Unit -> Unit
affineUnit newZero unit = { unit | zero <- newZero }

{-| Converts a quantity in one unit to another unit. Returns Nothing if the units are not compatible.

Example of a valid conversion:

    meter = baseLength
    cubicMeter = meter `mul` meter `mul` meter

    mile = scale 1609.34 meter
    gallon = scale 0.00378541178 cubicMeter
    mpg = mile `mul` (inv gallon)
    
    liter = scale 0.001 cubicMeter
    kilometer = scale 1000 meter
    kmpl = kilometer `mul` (inv liter)
    
    result = convert 30 mpg kmpl
        (result ~== Just 12.754)

Example of an invalid conversion:

    meter = baseLength
    kilogram = baseMass
    result = convert 30 meter kilogram
        (result == Nothing)
-}
convert : number -> Unit -> Unit -> Maybe Float
convert fromUnitQuantity fromUnit toUnit =
    let
        compatible : Unit -> Unit -> Bool
        compatible x y =
            x.len == y.len &&
            x.time == y.time &&
            x.mass == y.mass &&
            x.temp == y.temp &&
            x.amount == y.amount &&
            x.current == y.current &&
            x.lumInt == y.lumInt
    in
        if (compatible fromUnit toUnit) then Just ( ((fromUnitQuantity *
           (fromUnit.prefix) + fromUnit.zero) - toUnit.zero) / toUnit.prefix)
        else Nothing 
