module Test.Day01 exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Day01.A as A
import Day01.B as B




suite : Test
suite =
  describe "Day01"
    [ describe "A"
      [ test "something" <| \_ ->
        A.solve "123"
          |> Expect.equal "123"
      ]
    , describe "B"
      [ test "something" <| \_ ->
        B.solve "123"
          |> Expect.equal "123"
      ]
    ]
