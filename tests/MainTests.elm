module MainTests exposing (suite)

import Expect
import Main exposing (Flag(..), incrementCells)
import Matrix
import Maybe
import Test exposing (..)


suite : Test
suite =
    describe "The Main module"
        [ describe "incrementCells"
            [ test "increments and lights up cell on position (0, 0)" <|
                \_ ->
                    let
                        grid =
                            Matrix.createMatrix 3 3 ( Nothing, None )
                    in
                    incrementCells ( 0, 0 ) grid
                        |> Expect.equal
                            [ [ ( Just 1, LightUpYellow ), ( Nothing, None ), ( Nothing, None ) ]
                            , [ ( Nothing, None ), ( Nothing, None ), ( Nothing, None ) ]
                            , [ ( Nothing, None ), ( Nothing, None ), ( Nothing, None ) ]
                            ]
            , test "increments cells in column" <|
                \_ ->
                    let
                        grid =
                            Matrix.createMatrix 3 3 ( Nothing, None )
                    in
                    incrementCells ( 1, 1 ) grid
                        |> incrementCells ( 1, 0 )
                        |> Expect.equal
                            [ [ ( Nothing, None ), ( Just 1, LightUpYellow ), ( Nothing, None ) ]
                            , [ ( Nothing, None ), ( Just 2, LightUpYellow ), ( Nothing, None ) ]
                            , [ ( Nothing, None ), ( Nothing, None ), ( Nothing, None ) ]
                            ]
            ]
        ]
