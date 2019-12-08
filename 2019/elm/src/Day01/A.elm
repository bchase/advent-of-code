module Day01.A exposing (..)

import Helpers exposing (wrap, fromJust)

main = wrap solve



solve : String -> String
solve input =
  input
    |> parse
    |> solve_
    |> String.fromInt


solve_ : List Int -> Int
solve_ = solve__ requiredFuelForMass


solve__ : (Int -> Int) -> List Int -> Int
solve__ calc masses =
  masses
    |> List.map calc
    |> List.sum


parse : String -> List Int
parse input =
  input
    |> String.trim
    |> String.split "\n"
    |> List.map (String.toInt >> fromJust)


requiredFuelForMass : Int -> Int
requiredFuelForMass mass =
  ((toFloat mass / 3) - 2) |> floor
