module Test.Day02 exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Day02.A as A
import Day02.B as B




suite : Test
suite =
  describe "Day02"
    [ describe "A.execute"
      [ test "case 1" <| \_ ->
        A.test "1,0,0,0,99"
          |> Expect.equal "2,0,0,0,99"
      , test "case 2" <| \_ ->
        A.test "2,3,0,3,99"
          |> Expect.equal "2,3,0,6,99"
      , test "case 3" <| \_ ->
        A.test "2,4,4,5,99,0"
          |> Expect.equal "2,4,4,5,99,9801"
      , test "case 4" <| \_ ->
        A.test "1,1,1,4,99,5,6,0,99"
          |> Expect.equal "30,1,1,4,2,5,6,0,99"
      ]
    ]
