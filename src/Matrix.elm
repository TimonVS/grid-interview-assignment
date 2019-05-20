module Matrix exposing (Matrix, Point, createMatrix, indexedMapRows, mapCells, mapRows)


type alias Matrix a =
    List (List a)


type alias Point =
    ( Int, Int )


createMatrix : Int -> Int -> a -> Matrix a
createMatrix width height initialValue =
    List.repeat 50 (List.repeat 50 initialValue)


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


mapRows : (List a -> List a) -> Matrix a -> Matrix a
mapRows fn matrix =
    List.map fn matrix


indexedMapRows : (Int -> List a -> List a) -> Matrix a -> Matrix a
indexedMapRows fn matrix =
    List.indexedMap fn matrix
