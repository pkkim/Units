module SI where

{-| SI units, metric prefixes, and other common SI-based units.

# SI units
@docs meter, metre, second, kilogram, kilogramme, kelvin, mole, ampere, candela

# Prefixes
@docs yotta, zetta, exa, peta, tera, giga, mega, kilo, hecto, deca, deci,
centi, milli, micro, nano, pico, femto, atto, zepto, yocto

# Utility unit
@docs gram

# Other spellings
@docs metre, kilogramme, gramme

# SI named derived units
@docs hertz, radian, steradian, newton, pascal, joule, watt, coulomb, volt,
farad, ohm siemens, weber, tesla, henry, degree, lumen, lux, becquerel, gray,
sievert, katal
-}

import Units (..)
import Ratio (..)

-- SI base units.
{-| SI unit of length. -}
meter = baseLength
{-| SI unit of time. -}
second = baseTime
{-| SI unit of mass. -}
kilogram = baseMass
{-| SI unit of temperature. -}
kelvin = baseTemp
{-| SI unit of amount. -}
mole = baseAmount
{-| SI unit of electric current. -}
ampere = baseCurrent
{-| SI unit of luminous intensity. -}
candela = baseLumInt

-- Metric prefixes to scale units by some factor.
{-| Scales unit prefix by 10^24 -}
yotta : Unit -> Unit
yotta = scale (10 ^ 24)
{-| Scales unit prefix by 10^21 -}
zetta : Unit -> Unit
zetta = scale (10 ^ 21)
{-| Scales unit prefix by 10^18 -}
exa : Unit -> Unit
exa = scale (10 ^ 18)
{-| Scales unit prefix by 10^15 -}
peta : Unit -> Unit
peta = scale (10 ^ 15)
{-| Scales unit prefix by 10^12 -}
tera : Unit -> Unit
tera = scale (10 ^ 12)
{-| Scales unit prefix by 10^9 -}
giga : Unit -> Unit
giga = scale (10 ^ 9)
{-| Scales unit prefix by 10^6 -}
mega : Unit -> Unit
mega = scale (10 ^ 6)
{-| Scales unit prefix by 10^3 -}
kilo : Unit -> Unit
kilo = scale (10 ^ 3)
{-| Scales unit prefix by 10^2 -}
hecto : Unit -> Unit
hecto = scale (10 ^ 2)
{-| Scales unit prefix by 10 -}
deca : Unit -> Unit
deca = scale 10 
{-| Scales unit prefix by 10^-1 -}
deci : Unit -> Unit
deci = scale (10 ^ -1)
{-| Scales unit prefix by 10^-2 -}
centi : Unit -> Unit
centi = scale (10 ^ -2)
{-| Scales unit prefix by 10^-3 -}
milli : Unit -> Unit
milli = scale (10 ^ -3)
{-| Scales unit prefix by 10^-6 -}
micro : Unit -> Unit
micro = scale (10 ^ -6)
{-| Scales unit prefix by 10^-9 -}
nano : Unit -> Unit
nano = scale (10 ^ -9)
{-| Scales unit prefix by 10^-12 -}
pico : Unit -> Unit
pico = scale (10 ^ -12)
{-| Scales unit prefix by 10^-15 -}
femto : Unit -> Unit
femto = scale (10 ^ -15)
{-| Scales unit prefix by 10^-18 -}
atto : Unit -> Unit
atto = scale (10 ^ -18)
{-| Scales unit prefix by 10^-21 -}
zepto : Unit -> Unit
zepto = scale (10 ^ -21)
{-| Scales unit prefix by 10^-24 -}
yocto : Unit -> Unit
yocto = scale (10 ^ -24)

-- Kilogram is the SI unit of mass, but we prefix "gram," not "kilogram."
-- Defining "gram" allows us to say "milli gram," etc.
{-| More commonly prefixed unit of mass. -}
gram = milli kilogram

-- Some units have more than one spelling.
{-| Alternate spelling of meter. -}
metre = meter
{-| Alternate spelling of kilogram. -}
kilogramme = kilogram
{-| Alternate spelling of gram. -}
gramme = gram

-- SI named derived units.
{-| Unit for frequency equal to one per second. -}
hertz = inv second
{-| Unit for angle measure defined as the angle subtended on a circle of unit
radius by an arc of unit length. Equal to 1/(2 pi) turns around a circle. -}
radian = unity
{-| Unit for solid angle measure defined as the solid angle subtended on a
sphere of unit radius by a unit area on its surface. 4 pi steradians subtend a
sphere. -}
steradian = unity
{-| SI derived unit of force. -}
newton = (kilogram `mul` meter) `per` (second `pow` (fromInt 2))
{-| SI derived unit of pressure. -}
pascal = newton `per` (second `pow` (fromInt 2))
{-| SI derived unit of energy. -}
joule = newton `mul` meter
{-| SI derived unit of power. -}
watt = joule `per` second
{-| SI derived unit of charge. -}
coulomb = second `mul` ampere
{-| SI derived unit of voltage. -}
volt = watt `per` ampere
{-| SI derived unit of capacitance. -}
farad = coulomb `per` volt
{-| SI derived unit of electric resistance. -}
ohm = volt `per` ampere
{-| SI derived unit of electric conductance. -}
siemens = inv ohm
{-| SI derived unit of magnetic flux. -}
weber = joule `per` ampere
{-| SI derived unit of magnetic flux density. -}
tesla = weber `per` (meter `pow` (fromInt 2))
{-| SI derived unit of inductance. -}
henry = volt `mul` second `per` ampere
{-| SI derived unit of temperature, whose units are the same size as Kelvin and
whose zero is 273.15 kelvin above absolute zero. -}
degree = affineUnit 273.15 kelvin
{-| SI derived unit of luminous intensity. -}
lumen = candela `mul` steradian
{-| SI derived unit of luminous flux per area. -}
lux = lumen `per` (meter `pow` fromInt 2)
{-| SI derived unit of radioactivity. -}
becquerel = inv second
{-| SI derived unit of ionizing radiation dose. -}
gray = joule `per` kilogram
{-| SI derived unit of ionizing radiation dose, measuring health effects on the
human body. -}
sievert = joule `per` kilogram
{-| SI derived unit of catalytic activity. -}
katal = mole `per` second

