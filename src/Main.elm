module Main exposing (Msg(..), main, update, view)

import Browser
import Fibonacci exposing (isFibonacci, isFibonacciSequence, isNextFibonacci)
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, Point, mapCells)
import Maybe
import Task
import Time
import Tuple
import Utils exposing (delay, mapSequence)


type alias Grid =
    Matrix Cell


type alias CellValue =
    Int


type Flag
    = None
    | LightUpYellow
    | LightUpGreen


type alias Cell =
    ( CellValue, Flag )



-- MODEL


type alias Model =
    { grid : Grid
    , lastUpdated : Time.Posix
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model (Matrix.createMatrix 50 50 ( -1, None )) (Time.millisToPosix 0), Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ table []
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
        ]



-- UPDATE


type Msg
    = CellClick Point
    | TimeUpdate Time.Posix
    | ResetFlags Time.Posix


incrementCells : Grid -> Point -> Grid
incrementCells matrix ( x, y ) =
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


lightUpFibonacciSequences : Grid -> Grid
lightUpFibonacciSequences grid =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClick ( x, y ) ->
            ( { model | grid = incrementCells model.grid ( x, y ) |> lightUpFibonacciSequences }
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
