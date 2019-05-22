module Main exposing (Flag(..), Msg(..), incrementCells, main, update, view)

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
    Maybe Int


type Flag
    = None
    | LightUpYellow
    | LightUpGreen


type alias Cell =
    ( CellValue, Flag )



-- MODEL


type alias Model =
    { grid : Grid
    , timeLastClicked : Time.Posix
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model (Matrix.repeat 50 50 ( Nothing, None )) (Time.millisToPosix 0), Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ table []
            (List.indexedMap
                (\rowIndex row ->
                    tr []
                        (List.indexedMap
                            (\colIndex ( value, flag ) ->
                                td
                                    [ onClick (CellClick ( colIndex, rowIndex ))
                                    , class "cell"
                                    , class
                                        (case flag of
                                            LightUpYellow ->
                                                "light-up-yellow"

                                            LightUpGreen ->
                                                "light-up-green"

                                            _ ->
                                                ""
                                        )
                                    ]
                                    [ text
                                        (case value of
                                            Just x ->
                                                String.fromInt x

                                            _ ->
                                                ""
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
    | TimeLastClickedUpdate Time.Posix
    | ResetFlags Time.Posix


incrementCells : Point -> Grid -> Grid
incrementCells ( x, y ) matrix =
    mapCells
        (\cell ( colIndex, rowIndex ) ->
            case cell of
                ( Just value, flag ) ->
                    if colIndex == x || rowIndex == y then
                        ( Just (value + 1), LightUpYellow )

                    else
                        ( Just value, flag )

                _ ->
                    if colIndex == x || rowIndex == y then
                        ( Just 1, LightUpYellow )

                    else
                        ( Nothing, None )
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
                (\l -> isFibonacciSequence (List.map (\x -> Tuple.first x |> Maybe.withDefault -1) l))
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
                    ( Nothing, None )

                _ ->
                    ( value, None )
        )
        matrix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClick ( x, y ) ->
            ( { model | grid = incrementCells ( x, y ) model.grid |> lightUpFibonacciSequences }
            , Task.perform TimeLastClickedUpdate Time.now
            )

        TimeLastClickedUpdate time ->
            ( { model | timeLastClicked = time }, delay 1000 (ResetFlags time) )

        ResetFlags time ->
            ( { model
                | grid =
                    if Time.posixToMillis model.timeLastClicked == Time.posixToMillis time then
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
