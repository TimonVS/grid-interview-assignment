module Matrix exposing (Matrix, Point, createMatrix, mapCells)


type alias Matrix a =
    List (List a)


type alias Point =
    ( Int, Int )


createMatrix : Int -> Int -> a -> Matrix a
createMatrix width height initialValue =
    List.repeat height (List.repeat width initialValue)


mapCells : (a -> Point -> a) -> Matrix a -> Matrix a
mapCells fn matrix =
    List.indexedMap
        (\rowIndex row ->
            List.indexedMap
                (\colIndex cell ->
                    fn cell ( colIndex, rowIndex )
                )
                row
        )
        matrix
