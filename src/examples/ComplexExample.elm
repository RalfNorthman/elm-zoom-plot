module Main exposing (main)

import Browser
import Browser.Dom exposing (Error, Viewport, getViewport)
import Browser.Events
import Color exposing (toRgba)
import Data
import DateFormat.Language
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import FormatNumber
import FormatNumber.Locales
import Html exposing (Html)
import LineChart
import LineChart.Colors as Colors
import LineChart.Coordinate
import LineChart.Dots as Dots
import Maybe.Extra
import Task exposing (Task)
import Time exposing (Month(..), Posix, millisToPosix, posixToMillis)
import Time.Extra
import ZoomPlot as Plot


type alias Record =
    { date : Posix
    , ben : Maybe Float
    , ch4 : Maybe Float
    , co : Maybe Float
    , ebe : Maybe Float
    , nmhc : Maybe Float
    , no : Maybe Float
    , no_2 : Maybe Float
    , nox : Maybe Float
    , o_3 : Maybe Float
    , pm10 : Maybe Float
    , pm25 : Maybe Float
    , so_2 : Maybe Float
    , tch : Maybe Float
    , tol : Maybe Float
    , station : Int
    }


type alias Point =
    { x : Float, y : Float }


noRecords : List Record
noRecords =
    let
        from : Int
        from =
            Time.Extra.Parts 2018 Mar 1 13 0 0 0
                |> Time.Extra.partsToPosix Time.utc
                |> posixToMillis

        to : Int
        to =
            Time.Extra.Parts 2018 Mar 15 2 0 0 0
                |> Time.Extra.partsToPosix Time.utc
                |> posixToMillis
    in
    Data.records
        |> List.filter
            (\r -> r.no |> Maybe.Extra.isJust)
        |> List.filter
            (\r -> (posixToMillis r.date > from) && (posixToMillis r.date < to))



---- MODEL ----


type alias Model =
    { plot : Plot.State Record
    }


init : ( Model, Cmd msg )
init =
    ( { plot = Plot.init }, Cmd.none )



---- UPDATE ----


type Msg
    = ToPlot (Plot.Msg Record)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ToPlot plotMsg ->
            ( { model | plot = Plot.update plotMsg model.plot }, Cmd.none )



---- PROGRAM ----


myPointDecoder : Point -> Record
myPointDecoder { x, y } =
    Record (x |> round |> millisToPosix) Nothing Nothing Nothing Nothing Nothing (Just y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0


myWidth =
    1500


myHeight =
    800


view : Model -> Html Msg
view model =
    layout [ padding 20 ] <|
        el
            [ width <| px myWidth
            , height <| px myHeight
            ]
            (Plot.custom
                { lines =
                    [ LineChart.line
                        Colors.rust
                        Dots.circle
                        ""
                        noRecords
                    ]
                , toMsg = ToPlot
                , xAcc = .date >> posixToMillis >> toFloat
                , yAcc = .no >> Maybe.withDefault 0
                , pointDecoder = myPointDecoder
                }
                |> Plot.width myWidth
                |> Plot.height myHeight
                |> Plot.xIsTime True
                |> Plot.marginRight 50
                |> Plot.marginTop 50
                |> Plot.draw model.plot
            )


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
