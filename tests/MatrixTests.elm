module MatrixTests exposing (suite)

import Expect
import Matrix
import Test exposing (..)


suite : Test
suite =
    describe "The Matrix module"
        [ describe "Matrix.repeat"
            [ test "create matrix of 3x3" <|
                \_ ->
                    Matrix.repeat 3 3 -1
                        |> Expect.equal
                            [ [ -1, -1, -1 ]
                            , [ -1, -1, -1 ]
                            , [ -1, -1, -1 ]
                            ]
            ]
        ]
