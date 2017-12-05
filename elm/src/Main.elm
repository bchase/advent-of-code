module Main exposing (..)

import Platform exposing (programWithFlags)
import Task
import Json.Decode
import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)
import Ports exposing (exit)


dayXXa : String -> String
dayXXa input =
  "received input: " ++ input


solve : Input -> String
solve { mode, ab, input } =
  case ( mode, ab ) of
    ( Run, A ) ->
      dayXXa input

    ( Run, B ) ->
      "Not implemented"

    ( Test, A ) ->
      "Not implemented"

    ( Test, B ) ->
      "Not implemented"


type Msg
  = Start


update : Msg -> Input -> ( (), Cmd Msg )
update msg input =
  case msg of
    Start ->
      () ! [ return <| solve input ]



-- boilerplate


main : Program Flags Model Msg
main =
  programWithFlags
    { init = init
    , update = parseInputAnd update
    , subscriptions = \_ -> Sub.none
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
  parseFlags flags ! [ cmd Start ]



-- helpers


success : String -> Output
success =
  Output True


failure : String -> Output
failure =
  Output False


return : String -> Cmd Msg
return =
  exit << success


fail : String -> Cmd Msg
fail =
  exit << failure


cmd : msg -> Cmd msg
cmd msg =
  msg
    |> Task.succeed
    |> Task.perform identity


parseFlags : Flags -> Model
parseFlags ({ mode, ab, input } as flags) =
  let
    ab_ =
      case String.toUpper ab of
        "A" ->
          Just A

        "B" ->
          Just B

        _ ->
          Nothing

    mode_ =
      case String.toUpper mode of
        "RUN" ->
          Just Run

        "TEST" ->
          Just Test

        _ ->
          Nothing

    model =
      Maybe.map3 (Input) mode_ ab_ (Just input)
  in
    Result.fromMaybe flags model


parseInputAnd : (Msg -> Input -> ( (), Cmd Msg )) -> Msg -> Model -> ( Model, Cmd Msg )
parseInputAnd update msg model =
  case ( msg, model ) of
    ( _, Err flags ) ->
      model ! [ fail <| "Unable to parse flags" ++ toString flags ]

    ( msg, Ok input ) ->
      let
        cmd =
          Tuple.second <| update msg input
      in
        model ! [ cmd ]
