module Year2017.Day04 exposing (..)

import String as S
import List as L
import List.Extra as L
import Types exposing (Input, Mode(..), AB(..))
import Helpers exposing (defaultMain)


noDuplicateTokens : String -> Bool
noDuplicateTokens =
  allTokensUniq << S.words


noDuplicateAnagramTokens : String -> Bool
noDuplicateAnagramTokens =
  allTokensUniq << L.map (S.join "" << L.sort << S.split "") << S.words


allTokensUniq : List String -> Bool
allTokensUniq tokens =
  L.length tokens == L.length (L.unique tokens)


solve : Input -> String
solve { mode, ab, input } =
  let
    ps =
      input
        |> S.split "\\n"
        |> L.filter (\p -> p /= "")
  in
    case ( mode, ab ) of
      ( Test, A ) ->
        toString <| noDuplicateTokens input

      ( Test, B ) ->
        toString <| noDuplicateAnagramTokens input

      ( Run, A ) ->
        ps
          |> L.filter noDuplicateTokens
          |> L.length
          |> toString

      ( Run, B ) ->
        ps
          |> L.filter noDuplicateAnagramTokens
          |> L.length
          |> toString


main =
  defaultMain solve
