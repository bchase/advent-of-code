module DayWithoutUpdate exposing (..)

import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)
import Helpers


solve : Input -> String
solve { mode, ab, input } =
  case ( mode, ab ) of
    ( _, _ ) ->
      (\str -> "received input: " ++ str) input


main =
  Helpers.defaultMain solve
