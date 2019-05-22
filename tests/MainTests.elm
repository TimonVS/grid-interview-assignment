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
            [ test "increments and lights up cell on the first row and first column" <|
                \_ ->
                    let
                        grid =
                            Matrix.repeat 3 3 ( Nothing, None )
                    in
                    incrementCells ( 0, 0 ) grid
                        |> Expect.equal
                            [ [ ( Just 1, LightUpYellow ), ( Just 1, LightUpYellow ), ( Just 1, LightUpYellow ) ]
                            , [ ( Just 1, LightUpYellow ), ( Nothing, None ), ( Nothing, None ) ]
                            , [ ( Just 1, LightUpYellow ), ( Nothing, None ), ( Nothing, None ) ]
                            ]
            , test "increments cells twice" <|
                \_ ->
                    let
                        grid =
                            Matrix.repeat 3 3 ( Nothing, None )
                    in
                    incrementCells ( 0, 0 ) grid
                        |> incrementCells ( 1, 1 )
                        |> Expect.equal
                            [ [ ( Just 1, LightUpYellow ), ( Just 2, LightUpYellow ), ( Just 1, LightUpYellow ) ]
                            , [ ( Just 2, LightUpYellow ), ( Just 1, LightUpYellow ), ( Just 1, LightUpYellow ) ]
                            , [ ( Just 1, LightUpYellow ), ( Just 1, LightUpYellow ), ( Nothing, None ) ]
                            ]
            ]
        ]
