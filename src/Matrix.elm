module Matrix exposing (Matrix, Point, map, repeat)


type alias Matrix a =
    List (List a)


type alias Point =
    ( Int, Int )


repeat : Int -> Int -> a -> Matrix a
repeat width height initialValue =
    List.repeat height (List.repeat width initialValue)


map : (a -> Point -> a) -> Matrix a -> Matrix a
map fn m =
    List.indexedMap
        (\rowIndex row ->
            List.indexedMap
                (\colIndex cell ->
                    fn cell ( colIndex, rowIndex )
                )
                row
        )
        m
