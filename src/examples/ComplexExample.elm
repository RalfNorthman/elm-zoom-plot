module Main exposing (main)

import Browser
import Browser.Dom exposing (Error, Viewport, getViewport)
import Browser.Events
import Color exposing (toRgba)
import DateFormat.Language
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import FormatNumber
import FormatNumber.Locales
import Html exposing (Html)
import Html.Attributes
import LineChart
import LineChart.Colors as Colors
import LineChart.Coordinate
import LineChart.Dots as Dots
import Plot
import Task exposing (Task)
import Time exposing (Posix)



---- TEST-DATA ----


type alias Foobar =
    { foo : Posix
    , bar : Float
    , baz : String
    }


makeData : (Float -> Float) -> Float -> List Foobar
makeData func scale =
    let
        indexedValues : List ( Int, Float )
        indexedValues =
            List.indexedMap
                (\i x -> Tuple.pair i (toFloat x * scale))
            <|
                List.range -221 65

        addStringValue : Int -> String
        addStringValue index =
            case modBy 3 index of
                0 ->
                    "Wichita Vortex"

                1 ->
                    "KxKc 3"

                _ ->
                    "Bolobooz"

        pairToFoobar : ( Int, Float ) -> Foobar
        pairToFoobar ( i, x ) =
            Foobar
                (x
                    |> floor
                    |> Time.millisToPosix
                )
                (func <| x / scale)
                (addStringValue i)
    in
    List.map pairToFoobar indexedValues


polySinData : List Foobar
polySinData =
    makeData (\x -> 0.005 * x ^ 2 - 0.2 * x - 0.5 + 10 * sin x)
        100000


polyData : List Foobar
polyData =
    makeData (\x -> 0.00006 * x ^ 3 + 0.013 * x ^ 2 - 0.1 * x - 0.5)
        200000


poly3xSinData : List Foobar
poly3xSinData =
    makeData
        (\x -> 0.03 * x ^ 2 - 0.5 * x - 3.5 + 50 * sin (3 * x))
        10000000


cosData : List Foobar
cosData =
    makeData (\x -> 1000 + 100 * cos (x / 3))
        100000000



---- MODEL ----


type alias Model =
    { plot1 : Plot.State Foobar
    , plot2 : Plot.State Foobar
    , plot3 : Plot.State Foobar
    , plotWidth : Float
    , plotHeight : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { plot1 = Plot.init
      , plot2 = Plot.init
      , plot3 = Plot.init
      , plotWidth = 0
      , plotHeight = 0
      }
    , getInitialBrowserSize
    )


plotConfig : List (LineChart.Series Foobar) -> Plot.Config Foobar
plotConfig lines =
    let
        default =
            Plot.defaultConfigWith
                []
                (.foo >> Time.posixToMillis >> toFloat)
                .bar
                myPointDecoder
    in
    { default
        | lines = lines
        , xIsTime = True
        , labelFunc = myLabelFunc
        , language = DateFormat.Language.swedish
        , numberFormat =
            \x ->
                FormatNumber.format FormatNumber.Locales.frenchLocale x
        , margin =
            { top = 20
            , right = 0
            , bottom = 30
            , left = 70
            }
    }


myPointDecoder : LineChart.Coordinate.Point -> Foobar
myPointDecoder { x, y } =
    Foobar (x |> floor |> Time.millisToPosix) y ""


myLabelFunc : Foobar -> String
myLabelFunc foobar =
    foobar.baz



---- UPDATE ----


type PlotNr
    = Plot1
    | Plot2
    | Plot3


type Msg
    = ToPlot PlotNr (Plot.Msg Foobar)
    | BrowserResize Int Int
    | InitialBrowserSize (Result Error Viewport)


getInitialBrowserSize : Cmd Msg
getInitialBrowserSize =
    getViewport
        |> Task.attempt InitialBrowserSize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToPlot Plot1 plotMsg ->
            ( { model | plot1 = Plot.update (plotConfig lines1) plotMsg model.plot1 }
            , Cmd.none
            )

        ToPlot Plot2 plotMsg ->
            ( { model | plot2 = Plot.update (plotConfig lines2) plotMsg model.plot2 }
            , Cmd.none
            )

        ToPlot Plot3 plotMsg ->
            ( { model | plot3 = Plot.update (plotConfig lines3) plotMsg model.plot3 }
            , Cmd.none
            )

        BrowserResize width height ->
            ( updatePlotDimensions model (toFloat width) (toFloat height)
            , Cmd.none
            )

        InitialBrowserSize result ->
            case result of
                Ok { viewport } ->
                    ( updatePlotDimensions
                        model
                        viewport.width
                        viewport.height
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


updatePlotDimensions : Model -> Float -> Float -> Model
updatePlotDimensions model width height =
    let
        realEstate : Float -> Float
        realEstate dimension =
            dimension - boxSize - 2 * layoutPadding
    in
    { model
        | plotWidth = realEstate width
        , plotHeight = realEstate height / 3 - 5
    }



---- VIEW ----


myLine color shape title data =
    LineChart.line color shape title data


lines1 : List (LineChart.Series Foobar)
lines1 =
    [ myLine Colors.goldLight Dots.diamond "polysin" polySinData
    , myLine Colors.greenLight Dots.square "poly" polyData
    ]


lines2 : List (LineChart.Series Foobar)
lines2 =
    [ myLine Colors.greenLight Dots.square "poly" polyData
    , myLine Colors.goldLight Dots.diamond "polysin" polySinData
    , myLine Colors.pinkLight Dots.plus "poly3xsin" poly3xSinData
    ]


lines3 : List (LineChart.Series Foobar)
lines3 =
    [ myLine Colors.tealLight Dots.circle "cos" cosData
    , myLine Colors.goldLight Dots.diamond "polysin" polySinData
    , myLine Colors.pinkLight Dots.plus "poly3xsin" poly3xSinData
    ]


col : Color.Color -> Element.Color
col color =
    fromRgb (toRgba color)


greyBox : Element Msg
greyBox =
    el
        [ width <| px boxSize
        , height <| px boxSize
        , Background.color <| col Colors.grayLightest
        , Border.rounded 20
        , Border.width 1
        , Border.color <| col Colors.grayLight
        ]
        none


invisibleBox : Element Msg
invisibleBox =
    el
        [ width <| px boxSize
        , height <| px boxSize
        ]
        none


boxSize =
    150


layoutPadding =
    10


view : Model -> Html Msg
view model =
    let
        myDraw lines acc rout =
            Plot.draw model.plotWidth
                model.plotHeight
                (plotConfig lines)
                (acc model)
                (\msg -> ToPlot rout msg)
    in
    Element.layout
        [ width fill
        , height fill
        , padding layoutPadding
        ]
    <|
        row
            [ width fill
            , height fill
            ]
            [ column
                [ width fill
                , height fill
                ]
                [ el [ centerX ] greyBox
                , column
                    [ width fill
                    , height fill
                    ]
                    [ myDraw lines1 .plot1 Plot1
                    , myDraw lines2 .plot2 Plot2
                    , myDraw lines3 .plot3 Plot3
                    ]
                ]
            , column
                [ height fill ]
                [ el [ alignTop ] invisibleBox
                , el [ centerY ] greyBox
                ]
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize BrowserResize



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
