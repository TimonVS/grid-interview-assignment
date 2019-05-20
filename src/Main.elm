module Main exposing (Msg(..), main, update, view)

import Browser
import Fibonacci exposing (isFibonacci, isFibonacciSequence, isNextFibonacci)
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, Point, mapCells)
import Maybe
import Process
import Task
import Time
import Tuple


type alias Grid =
    Matrix Cell


type alias Model =
    { grid : Grid
    , lastUpdated : Time.Posix
    }


type Flag
    = None
    | LightUpYellow
    | LightUpGreen


type alias CellValue =
    Int


type alias Cell =
    ( CellValue, Flag )


type Msg
    = CellClick Point
    | TimeUpdate Time.Posix
    | ResetFlags Time.Posix


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


init : () -> ( Model, Cmd msg )
init _ =
    ( Model (Matrix.createMatrix 50 50 ( -1, None )) (Time.millisToPosix 0), Cmd.none )


incrementCell : Grid -> Point -> Grid
incrementCell matrix ( x, y ) =
    mapCells
        (\( cellValue, _ ) ( colIndex, rowIndex ) ->
            if ( colIndex, rowIndex ) == ( x, y ) && cellValue == -1 then
                ( 1, LightUpYellow )

            else if cellValue == -1 then
                ( cellValue, None )

            else if colIndex == x || rowIndex == y then
                ( cellValue + 1, LightUpYellow )

            else
                ( cellValue, None )
        )
        matrix


resetFlags : Grid -> Grid
resetFlags matrix =
    mapCells
        (\( value, flag ) _ ->
            case ( value, flag ) of
                ( _, LightUpGreen ) ->
                    ( -1, None )

                _ ->
                    ( value, None )
        )
        matrix


isInfixOf : List a -> List a -> Bool
isInfixOf seq list =
    case ( seq, list ) of
        ( [], [] ) ->
            True

        ( _, [] ) ->
            False

        ( _, y :: ys ) ->
            if List.take (List.length seq) list == seq then
                True

            else
                isInfixOf seq ys


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClick ( x, y ) ->
            ( { model
                | grid =
                    incrementCell model.grid ( x, y )
                        |> (\grid ->
                                List.map
                                    (\row ->
                                        mapSequence
                                            (\( value, flag ) isPartOfFibSequence ->
                                                ( value
                                                , if isPartOfFibSequence then
                                                    LightUpGreen

                                                  else
                                                    flag
                                                )
                                            )
                                            (\l -> isFibonacciSequence (List.map Tuple.first l))
                                            5
                                            row
                                    )
                                    grid
                           )
              }
            , Task.perform TimeUpdate Time.now
            )

        TimeUpdate time ->
            ( { model | lastUpdated = time }, delay 1000 (ResetFlags time) )

        ResetFlags time ->
            ( { model
                | grid =
                    if Time.posixToMillis model.lastUpdated == Time.posixToMillis time then
                        resetFlags model.grid

                    else
                        model.grid
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    table []
        (List.indexedMap
            (\rowIndex row ->
                tr []
                    (List.indexedMap
                        (\colIndex ( cell, flag ) ->
                            td
                                [ onClick (CellClick ( colIndex, rowIndex ))
                                , class "cell"
                                , class
                                    (if flag == LightUpYellow then
                                        "light-up-yellow"

                                     else if flag == LightUpGreen then
                                        "light-up-green"

                                     else
                                        ""
                                    )
                                ]
                                [ text
                                    (case cell + 1 of
                                        0 ->
                                            ""

                                        _ ->
                                            String.fromInt cell
                                    )
                                ]
                        )
                        row
                    )
            )
            model.grid
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
