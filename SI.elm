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
meter = baseLength
second = baseTime
kilogram = baseMass
kelvin = baseTemp
mole = baseAmount
ampere = baseCurrent
candela = baseLumInt

-- Metric prefixes to scale units by some factor.
yotta = scale (10 ^ 24)
zetta = scale (10 ^ 21)
exa = scale (10 ^ 18)
peta = scale (10 ^ 15)
tera = scale (10 ^ 12)
giga = scale (10 ^ 9)
mega = scale (10 ^ 6)
kilo = scale (10 ^ 3)
hecto = scale (10 ^ 2)
deca = scale 10 
deci = scale (10 ^ -1)
centi = scale (10 ^ -2)
milli = scale (10 ^ -3)
micro = scale (10 ^ -6)
nano = scale (10 ^ -9)
pico = scale (10 ^ -12)
femto = scale (10 ^ -15)
atto = scale (10 ^ -18)
zepto = scale (10 ^ -21)
yocto = scale (10 ^ -24)

-- Kilogram is the SI unit of mass, but we prefix "gram," not "kilogram."
-- Defining "gram" allows us to say "milli gram," etc.
gram = milli kilogram

-- Some units have more than one spelling.
metre = meter
kilogramme = kilogram
gramme = gram

-- SI named derived units.
hertz = inv second
radian = unity
steradian = unity
newton = (kilogram `mul` meter) `per` (second `pow` (fromInt 2))
pascal = newton `per` (second `pow` (fromInt 2))
joule = newton `mul` meter
watt = joule `per` second
coulomb = second `mul` ampere
volt = watt `per` ampere
farad = coulomb `per` volt
ohm = volt `per` ampere
siemens = inv ohm
weber = joule `per` ampere
tesla = volt `mul` second `per` (meter `pow` (fromInt 2))
henry = volt `mul` second `per` ampere
degree = affineUnit 273.15 kelvin
lumen = candela `mul` steradian
lux = lumen `per` (meter `pow` fromInt 2)
becquerel = inv second
gray = joule `per` kilogram
sievert = joule `per` kilogram
katal = mole `per` second

