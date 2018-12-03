module AoC201704A exposing (..)


a1 : List String -> Int
a1 lines =
  let
    valid : String -> Bool
    valid passphrase =
      List.length (S.split " " passphrase) == List.length (L.unique (S.split " " passphrase))
  in
    L.length (L.filter valid lines)


a2 : List String -> Int
a2 lines =
  -- refactor into functions
  let
    noDuplicateTokens : String -> Bool
    noDuplicateTokens passphrase =
      allTokensUniq << S.words <| passphrase

    allTokensUniq : List String -> Bool
    allTokensUniq tokens =
      L.length tokens == L.length (L.unique tokens)
  in
    L.length (L.filter noDuplicateTokens lines)


a3 : List String -> Int
a3 lines =
  let
    noDuplicateTokens : String -> Bool
    noDuplicateTokens =
      allTokensUniq << S.words

    allTokensUniq : List String -> Bool
    allTokensUniq tokens =
      L.length tokens == L.length (L.unique tokens)
  in
    -- -- you can pipe this, which is sort of The Elm Way, but also...
    -- L.length <| L.filter noDuplicateTokens lines
    lines
      |> L.filter noDuplicateTokens
      |> L.length


a4 : List String -> Int
a4 =
  let
    noDuplicateTokens : String -> Bool
    noDuplicateTokens =
      allTokensUniq << S.words

    allTokensUniq : List String -> Bool
    allTokensUniq tokens =
      L.length tokens == L.length (L.unique tokens)
  in
    -- this is the "point-free" way, which is more Haskell-like
    -- notice the `lines` param disappeared next to `a4`
    L.length << L.filter noDuplicateTokens


a5 : List String -> Int
a5 =
  -- and, if you're real sadomasochist...
  L.length << L.filter ((\t -> L.length t == L.length (L.unique t)) << S.words)
