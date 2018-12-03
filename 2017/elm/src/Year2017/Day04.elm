module Year2017.Day04 exposing (..)

import String as S
import List as L
import List.Extra as L
import Types exposing (Input, Mode(..), AB(..))
import Helpers


a : List String -> Int
a =
  L.length << L.filter ((\t -> L.length t == L.length (L.unique t)) << S.words)


solve : Input -> String
solve { mode, ab, input } =
  let
    lines : List String
    lines =
      Helpers.lines_ input
  in
    case ( mode, ab ) of
      ( Test, A ) ->
        "UNIMPLEMENTED"

      ( Test, B ) ->
        "UNIMPLEMENTED"

      ( Run, A ) ->
        toString <| a lines

      ( Run, B ) ->
        "UNIMPLEMENTED"


main =
  Helpers.defaultMain solve
