module Year2017.Day04 exposing (..)

import String as S
import List as L
import List.Extra as L
import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)
import Helpers


(.) =
  (<<)


($) =
  (<|)


noDuplicateTokens : String -> Bool
noDuplicateTokens =
  allTokensUniq . S.words


noDuplicateAnagramTokens : String -> Bool
noDuplicateAnagramTokens =
  allTokensUniq . L.map (S.join "" . L.sort . S.split "") . S.words


allTokensUniq : List String -> Bool
allTokensUniq tokens =
  L.length tokens == L.length (L.unique tokens)


solve : Input -> String
solve { mode, ab, input } =
  case ( mode, ab ) of
    ( Test, A ) ->
      toString $ noDuplicateTokens input

    ( Test, B ) ->
      toString $ noDuplicateAnagramTokens input

    ( _, _ ) ->
      (\str -> "NOT-IMPLEMENTED -- input: " ++ str) input


main =
  Helpers.defaultMain solve
