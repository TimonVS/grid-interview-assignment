module Utils exposing (delay, mapSequence)

import Process
import Task


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


mapSequence : (a -> Bool -> b) -> (List a -> Bool) -> Int -> List a -> List b
mapSequence fn sequenceConditionFn n list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                newList =
                    List.take n list
            in
            if List.length list >= n && sequenceConditionFn newList then
                List.map (\a -> fn a True) newList ++ mapSequence fn sequenceConditionFn n (List.drop n list)

            else
                fn x False :: mapSequence fn sequenceConditionFn n (List.drop 1 list)
