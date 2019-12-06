module Day01.A exposing (main, solve, requiredFuelForMass)

import Helpers exposing (wrap)

main = wrap solve



solve : String -> String
solve input =
  input
    |> String.split "\n"
    |> List.filter (\ str -> not (str == "") )
    |> List.map (String.toInt >> fromJust)
    |> List.map requiredFuelForMass
    |> List.sum
    |> String.fromInt


requiredFuelForMass : Int -> Int
requiredFuelForMass mass =
  toFloat mass / toFloat 3
    |> floor
    |> ((-) 2)
    |> negate
  -- div 3, round down, sub 2


fromJust : Maybe a -> a
fromJust mx = case mx of
  Just x  -> x
  Nothing -> Debug.todo ""

