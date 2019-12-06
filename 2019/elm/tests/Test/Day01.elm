module Test.Day01 exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Day01.A as A
import Day01.B as B



-- For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2


suite : Test
suite =
  describe "Day01"
    [ describe "A"
      [ test "requiredFuelForMass 1" <| \_ ->
        A.requiredFuelForMass 12
          |> Expect.equal 2
      , test "requiredFuelForMass 2" <| \_ ->
        A.requiredFuelForMass 14
          |> Expect.equal 2
      , test "requiredFuelForMass 3" <| \_ ->
        A.requiredFuelForMass 1969
          |> Expect.equal 654
      , test "requiredFuelForMass 4" <| \_ ->
        A.requiredFuelForMass 100756
          |> Expect.equal 33583
      ]
    -- , describe "B"
    --   [ test "something" <| \_ ->
    --     B.solve "123"
    --       |> Expect.equal "123"
    --   ]
    ]
