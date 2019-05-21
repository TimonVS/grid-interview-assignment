module MainTests exposing (suite)

import Expect
import Main exposing (Flag(..), incrementCells)
import Matrix
import Test exposing (..)


suite : Test
suite =
    describe "The Main module"
        [ describe "incrementCells"
            [ test "increments and lights up cell on position (0, 0)" <|
                \_ ->
                    let
                        grid =
                            Matrix.createMatrix 3 3 ( -1, None )
                    in
                    incrementCells grid ( 0, 0 )
                        |> Expect.equal
                            [ [ ( 1, LightUpYellow ), ( -1, None ), ( -1, None ) ]
                            , [ ( -1, None ), ( -1, None ), ( -1, None ) ]
                            , [ ( -1, None ), ( -1, None ), ( -1, None ) ]
                            ]
            , test "increments and lights up cell on position (1, 1)" <|
                \_ ->
                    let
                        grid =
                            Matrix.createMatrix 3 3 ( -1, None )
                    in
                    incrementCells grid ( 1, 1 )
                        |> Expect.equal
                            [ [ ( -1, None ), ( -1, None ), ( -1, None ) ]
                            , [ ( -1, None ), ( 1, LightUpYellow ), ( -1, None ) ]
                            , [ ( -1, None ), ( -1, None ), ( -1, None ) ]
                            ]
            ]
        ]
