module FibonacciTests exposing (suite)

import Expect
import Fibonacci exposing (fibonacciIndex, isFibonacci)
import Test exposing (..)


suite : Test
suite =
    describe "The Fibonacci module"
        [ describe "isFibonacci"
            [ test "returns False for non-fibonacci number 4" <|
                \_ ->
                    isFibonacci 4 |> Expect.equal False
            , test "returns True for fibonacci number 5" <|
                \_ ->
                    isFibonacci 5 |> Expect.equal True
            ]
        , describe "returns nearest fibonacci index for number"
            [ test "returns 8 for fibonacci number 21" <|
                \_ -> fibonacciIndex 21 |> Expect.equal 8
            ]
        ]
