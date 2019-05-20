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
            , test "returns 0 for fibonacci number 0" <|
                \_ -> fibonacciIndex 0 |> Expect.equal 0
            , test "returns 2 for fibonacci number 1" <|
                \_ -> fibonacciIndex 1 |> Expect.equal 2
            ]
        , describe "isNextFibonacci"
            [ test "returns True for two consecutive fibonacci numbers 5 and 8" <|
                \_ -> isNextFibonacci 5 8 |> Expect.equal True
            , test "returns True for two consecutive fibonacci numbers 0 and 0" <|
                \_ -> isNextFibonacci 0 0 |> Expect.equal True
            , test "returns True for two consecutive fibonacci numbers 0 and 1" <|
                \_ -> isNextFibonacci 0 1 |> Expect.equal True
            , test "returns False for two non-consecutive fibonacci numbers 5 and 6" <|
                \_ -> isNextFibonacci 5 6 |> Expect.equal False
            ]
        ]
