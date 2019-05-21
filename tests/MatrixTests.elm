module MatrixTests exposing (suite)

import Expect
import Matrix exposing (createMatrix)
import Test exposing (..)


suite : Test
suite =
    describe "The Matrix module"
        [ describe "createMatrix"
            [ test "create matrix of 3x3" <|
                \_ ->
                    createMatrix 3 3 -1
                        |> Expect.equal
                            [ [ -1, -1, -1 ]
                            , [ -1, -1, -1 ]
                            , [ -1, -1, -1 ]
                            ]
            ]
        ]
