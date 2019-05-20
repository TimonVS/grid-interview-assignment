module FibonacciTests exposing (suite)

import Expect
import Fibonacci exposing (fibonacciIndex, isFibonacci, isNextFibonacci)
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
        , describe "fibonacciIndex"
            [ test "returns 8 for fibonacci number 21" <|
                \_ -> fibonacciIndex 21 |> Expect.equal 8
            ]
        , describe "isNextFibonacci"
            [ test "returns True for two consecutive fibonacci numbers 5 and 8" <|
                \_ -> isNextFibonacci 5 8 |> Expect.equal True
            , test "returns False for two non-consecutive fibonacci numbers 5 and 6" <|
                \_ -> isNextFibonacci 5 6 |> Expect.equal False
            ]
        ]
