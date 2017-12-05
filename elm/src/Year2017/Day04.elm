module Year2017.Day04 exposing (..)

import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)
import Helpers exposing (mainFor, return, fail)


solve : Input -> String
solve { mode, ab, input } =
  case ( mode, ab ) of
    ( _, _ ) ->
      (\str -> "received input: " ++ str) input


type Msg
  = Start


update : Msg -> Input -> ( (), Cmd Msg )
update msg input =
  case msg of
    Start ->
      () ! [ return <| solve input ]


main : Program Flags Model Msg
main =
  mainFor update Start
