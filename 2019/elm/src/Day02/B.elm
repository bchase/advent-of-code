module Day02.B exposing (main, solve)

import Array
import List.Extra as List
import Helpers exposing (wrap, fromJust)
import Day02.A exposing (Program, Instruction)
import Day02.A as A

main = wrap solve



solve : String -> String
solve input =
  let prog = A.parse input
   in case solve_ prog of
     Nothing ->
       "Nothing"
     Just (noun, verb) ->
       100 * noun + verb |> String.fromInt


solve_ : Program -> Maybe (Int, Int)
solve_ prog =
  let
    target = 19690720
    vals   = pairs 2000
  in
    vals
      |> List.find (eval prog >> ((==) target))


eval : Program -> (Int, Int) -> Int
eval prog initial  =
  prog
    |> A.prepare initial
    |> A.execProg 0
    |> Array.get 0
    |> fromJust



--- pair permutations ---

pairs : Int -> List (Int, Int)
pairs max = pairs_ max 0

pairs_ : Int -> Int -> List (Int, Int)
pairs_ max curr = case curr > max of
  True  -> []
  False ->
    let
      both = [ (curr, curr) ]

      lefts =
        if curr == 0
          then []
          else
            let ns = downToZero [ curr - 1 ]
             in List.zip ns (List.repeat (List.length ns) curr)

      rights =
        if curr == 0
         then []
         else
            let ns = downToZero [ curr - 1 ]
             in List.zip (List.repeat (List.length ns) curr) ns

    in
      both ++ lefts ++ rights ++ pairs_ max (curr + 1)

downToZero : List Int -> List Int
downToZero ns = case ns of
  []        -> ns
  0    :: _ -> ns
  head :: _ ->
    downToZero <| head - 1 :: ns
