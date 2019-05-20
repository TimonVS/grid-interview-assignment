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
    , timeLastClicked : Time.Posix
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
                                        (if value == -1 then
                                            ""

                                         else
                                            String.fromInt value
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


incrementCells : Grid -> Point -> Grid
incrementCells matrix ( x, y ) =
    mapCells
        (\( value, _ ) ( colIndex, rowIndex ) ->
            if ( colIndex, rowIndex ) == ( x, y ) && value == -1 then
                ( 1, LightUpYellow )

            else if value == -1 then
                ( value, None )

            else if colIndex == x || rowIndex == y then
                ( value + 1, LightUpYellow )

            else
                ( value, None )
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
