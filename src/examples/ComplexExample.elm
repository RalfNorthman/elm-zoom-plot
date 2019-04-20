module Main exposing (main)

import Browser
import Browser.Dom exposing (Error, Viewport, getViewport)
import Browser.Events
import Color exposing (toRgba)
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import LineChart
import LineChart.Colors as Colors
import LineChart.Coordinate
import LineChart.Dots as Dots
import Plot exposing (..)
import Task exposing (Task)



---- TEST-DATA ----


type alias Foobar =
    { foo : Float
    , bar : Float
    , baz : String
    }


makeData : (Float -> Float) -> Float -> List Foobar
makeData func scale =
    let
        indexedValues : List ( Int, Float )
        indexedValues =
            List.indexedMap
                (\i x -> Tuple.pair i (101001000 + toFloat x * scale))
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
            Foobar x (func x) (addStringValue i)
    in
    List.map pairToFoobar
        indexedValues


cosData : List Foobar
cosData =
    makeData cos 100000000


polyData : List Foobar
polyData =
    makeData (\x -> 0.01 * x * x - 0.1 * x - 0.5)
        200000


polySinData : List Foobar
polySinData =
    makeData (\x -> 0.01 * x ^ 2 - 0.1 * x - 0.5 + sin x)
        20000


poly3xSinData : List Foobar
poly3xSinData =
    makeData
        (\x -> 0.03 * x ^ 2 - 0.5 * x - 3.5 + 5 * sin (3 * x))
        10000000



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
    ( { plot1 = Plot.init <| plotConfig lines1
      , plot2 = Plot.init <| plotConfig lines2
      , plot3 = Plot.init <| plotConfig lines3
      , plotWidth = 0
      , plotHeight = 0
      }
    , getBrowserSize
    )


plotConfig : List (LineChart.Series Foobar) -> Plot.Config Foobar
plotConfig lines =
    { lines = lines
    , xIsTime = True
    , xAcc = .foo
    , yAcc = .bar
    , pointDecoder = pointDecoder
    , labelFunc = labelFunc
    , showLegends = False
    , xLabel = "Tid"
    , yLabel = ""
    }


pointDecoder : LineChart.Coordinate.Point -> Foobar
pointDecoder { x, y } =
    let
        emptyFoobar =
            Foobar 0 0 ""
    in
    { emptyFoobar | foo = x, bar = y }


labelFunc : Foobar -> String
labelFunc foobar =
    foobar.baz



---- UPDATE ----


type alias Id =
    String


type PlotNr
    = Plot1
    | Plot2
    | Plot3


type Msg
    = ToPlot PlotNr (Plot.Msg Foobar)
    | Resize Int Int
    | NewBrowserSize (Result Error Viewport)


getBrowserSize : Cmd Msg
getBrowserSize =
    getViewport
        |> Task.attempt NewBrowserSize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToPlot Plot1 plotMsg ->
            ( { model | plot1 = Plot.update plotMsg model.plot1 }
            , Cmd.none
            )

        ToPlot Plot2 plotMsg ->
            ( { model | plot2 = Plot.update plotMsg model.plot2 }
            , Cmd.none
            )

        ToPlot Plot3 plotMsg ->
            ( { model | plot3 = Plot.update plotMsg model.plot3 }
            , Cmd.none
            )

        Resize width height ->
            ( updatePlotDimensions model (toFloat width) (toFloat height)
            , Cmd.none
            )

        NewBrowserSize result ->
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
        realEstate dim =
            dim - thingSize - 2 * layoutPadding
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


thing : Element Msg
thing =
    el
        [ width <| px thingSize
        , height <| px thingSize
        , Background.color <| col Colors.grayLightest
        , Border.rounded 20
        , Border.width 1
        , Border.color <| col Colors.grayLight
        ]
        none


invisibleThing : Element Msg
invisibleThing =
    el
        [ width <| px 150
        , height <| px 150
        ]
        none


thingSize =
    150


layoutPadding =
    10


view : Model -> Html Msg
view model =
    let
        myDraw acc sub =
            draw model.plotWidth
                model.plotHeight
                (acc model)
                (\msg -> ToPlot sub msg)
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
                [ el [ centerX ] thing
                , column
                    [ width fill
                    , height fill
                    ]
                    [ myDraw .plot1 Plot1
                    , myDraw .plot2 Plot2
                    , myDraw .plot3 Plot3
                    ]
                ]
            , column
                [ height fill ]
                [ el [ alignTop ] invisibleThing
                , el [ centerY ] thing
                ]
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize Resize



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
