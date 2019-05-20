module Fibonacci exposing (fibonacciIndex, isFibonacci, isFibonacciSequence, isNextFibonacci)


isPerfectSquare : Float -> Bool
isPerfectSquare x =
    let
        s =
            sqrt x
    in
    s - toFloat (floor s) == 0


isFibonacci : Int -> Bool
isFibonacci x =
    let
        x1 =
            toFloat (5 * (x ^ 2) + 4)

        x2 =
            toFloat (5 * (x ^ 2) - 4)
    in
    isPerfectSquare x1 || isPerfectSquare x2


{-| Get the nearest fibonacci index

    fibonacciIndex 21 == 8

-}
fibonacciIndex : Int -> Int
fibonacciIndex x =
    let
        phi =
            1.61803399
    in
    case x of
        0 ->
            0

        _ ->
    round ((logBase e (toFloat x) + (logBase e 5 / 2)) / logBase e phi)


isNextFibonacci : Int -> Int -> Bool
isNextFibonacci x y =
    isFibonacci x && isFibonacci y && fibonacciIndex x + 1 == fibonacciIndex y


isFibonacciSequence : List Int -> Bool
isFibonacciSequence list =
    case list of
        [] ->
            False

        [ x, y ] ->
            isNextFibonacci x y

        x :: xs ->
            case List.head xs of
                Just y ->
                    if isNextFibonacci x y then
                        isFibonacciSequence xs

                    else
                        False

                Nothing ->
                    False
