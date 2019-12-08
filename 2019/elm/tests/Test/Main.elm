module Test.Main exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Day01.A as A01
import Day01.B as B01
import Day02.A as A02
import Day02.B as B02




suite : Test
suite =
  describe "2019"
    [ describe "Day01"
      [ describe "A"
        [ test "requiredFuelForMass 1" <| \_ ->
          A01.requiredFuelForMass 12
            |> Expect.equal 2
        , test "requiredFuelForMass 2" <| \_ ->
          A01.requiredFuelForMass 14
            |> Expect.equal 2
        , test "requiredFuelForMass 3" <| \_ ->
          A01.requiredFuelForMass 1969
            |> Expect.equal 654
        , test "requiredFuelForMass 4" <| \_ ->
          A01.requiredFuelForMass 100756
            |> Expect.equal 33583
        ]
      , describe "B"
        [ test "solve 1" <| \_ ->
          B01.solve "14"
            |> Expect.equal "2"
        , test "solve 2" <| \_ ->
          B01.solve "1969"
            |> Expect.equal "966"
        , test "solve 3" <| \_ ->
          B01.solve "100756"
            |> Expect.equal "50346"
        ]
      ]

    , describe "Day02"
      [ describe "A.execute"
        [ test "case 1" <| \_ ->
          A02.test "1,0,0,0,99"
            |> Expect.equal "2,0,0,0,99"
        , test "case 2" <| \_ ->
          A02.test "2,3,0,3,99"
            |> Expect.equal "2,3,0,6,99"
        , test "case 3" <| \_ ->
          A02.test "2,4,4,5,99,0"
            |> Expect.equal "2,4,4,5,99,9801"
        , test "case 4" <| \_ ->
          A02.test "1,1,1,4,99,5,6,0,99"
            |> Expect.equal "30,1,1,4,2,5,6,0,99"
        ]
      ]
    ]
