module TestUnits where

import Ratio (..)
import Units (..)

run : List Bool -> Maybe Int
run =
    let
        runRec : Int -> List Bool -> Maybe Int
        runRec k bools = case bools of
            (b::bs) -> if b then runRec (k+1) bs else Just k
            [] -> Nothing
    in
        runRec 0

withinRange val min max = case val of
    Just x -> min < x && x < max
    Nothing -> False

isNothing val = case val of
    Just _ -> False
    Nothing -> True

test1 =
    let
        meter : Unit
        meter = baseLength
        foot : Unit
        foot = scale 0.3048 meter
        result = convert 6 foot meter
    in
        withinRange result (6 * 0.3047) (6 * 0.3049)

test2 =
    let
        kelvin = baseTemp
        fahrenheit = affineUnit 255.372 (scale (5/9) kelvin)
        result = convert 300 kelvin fahrenheit
    in
        withinRange result 80.32 80.34

test3 =
    let
        kelvin = baseTemp
        celsius = affineUnit 273.15 kelvin
        reaumur = scale (100/80) celsius
        result = convert (-30) reaumur celsius
        fahrenheit = affineUnit 255.372 (scale (5/9) kelvin)
        result2 = convert (-30) reaumur fahrenheit
    in
        withinRange result -37.6 -37.4 &&
        withinRange result2 -35.6 -35.4

test4 =
    let
        meter = baseLength
        cubicMeter = meter `pow` (fromInt 3)
        mile = scale 1609.34 meter
        gallon = scale 0.00378541178 cubicMeter
        mpg = mile `per` gallon
        liter = scale 0.001 cubicMeter
        kilometer = scale 1000 meter
        kmpl = kilometer `per` liter
        result = convert 30 mpg kmpl
    in
        withinRange result 12.75 12.76

test5 =
    let
        meter = baseLength
        cubicMeter = meter `pow` (fromInt 3)
        mile = scale 1609.34 meter
        result = convert 5 mile cubicMeter
    in
        isNothing result

tests : List Bool
tests = [test1, False, test2, test3, test4, test5]
