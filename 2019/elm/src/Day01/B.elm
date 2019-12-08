module Day01.B exposing (main, solve)

import Helpers exposing (wrap, fromJust)
import Day01.A as A

main = wrap solve



solve : String -> String
solve input =
  input
    |> A.parse
    |> A.solve__ (A.requiredFuelForMass >> addFuelFuel)
    |> String.fromInt


addFuelFuel : Int -> Int
addFuelFuel fuel =
  fuel + fuelFuel fuel


fuelFuel : Int -> Int
fuelFuel orig =
  let
    extra =
      orig
        |> List.singleton
        |> A.solve_
  in
    if extra <= 0
      then 0
      else extra + fuelFuel extra
