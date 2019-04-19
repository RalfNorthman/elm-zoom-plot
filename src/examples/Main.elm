module Main exposing (Foobar, Id, Model, Msg(..), PlotNr(..), Point, col, cosData, expData, getBrowserSize, init, invisibleThing, labelFunc, layoutPadding, lines1, lines2, lines3, main, makeData, myLine, plotConfig, pointDecoder, poly3xSinData, polyData, polySinData, subscriptions, thing, thingSize, update, updatePlotDimensions, view)

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
import LineChart.Dots as Dots
import Plot exposing (..)
import Task exposing (Task)



---- TEST-DATA ----


type alias Foobar =
    { foo : Float
    , bar : Float
    , baz : String
    }


type alias Point =
    { x : Float, y : Float }


makeData : (Float -> Float) -> Float -> List Foobar
makeData func scale =
    let
        xs : List ( Int, Float )
        xs =
            List.indexedMap
                (\i x -> Tuple.pair i (101001000 + toFloat x * scale))
            <|
                List.range -221 65

        bazMaker : Int -> String
        bazMaker index =
            case modBy 3 index of
                0 ->
                    "Wichita Vortex"

                1 ->
                    "KxKc 3"

                _ ->
                    "Bolobooz"
    in
    List.map (\( i, x ) -> Foobar x (func x) <| bazMaker i) xs


expData : List Foobar
expData =
    makeData (\x -> 0.01 ^ (0.04 * x) + 3.5 * sin (2 * x))
        100000


cosData : List Foobar
cosData =
    makeData cos 1000000000


polyData : List Foobar
polyData =
    makeData (\x -> 0.01 * x * x - 0.1 * x - 0.5)
        2000000


polySinData : List Foobar
polySinData =
    makeData (\x -> 0.01 * x ^ 2 - 0.1 * x - 0.5 + sin x)
        2000000


poly3xSinData : List Foobar
poly3xSinData =
    makeData
        (\x -> 0.03 * x ^ 2 - 0.5 * x - 3.5 + 5 * sin (3 * x))
        100000000



---- MODEL ----


type alias Model =
    { plot1 : PlotState Foobar
    , plot2 : PlotState Foobar
    , plot3 : PlotState Foobar
    , plotWidth : Float
    , plotHeight : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { plot1 = plotInit <| plotConfig lines1
      , plot2 = plotInit <| plotConfig lines2
      , plot3 = plotInit <| plotConfig lines3
      , plotWidth = 0
      , plotHeight = 0
      }
    , getBrowserSize
    )


plotConfig : Lines Foobar -> PlotConfig Foobar
plotConfig lines =
    { lines = lines
    , xIsTime = True
    , xAcc = .foo
    , yAcc = .bar
    , pointDecoder = pointDecoder
    , labelFunc = labelFunc
    }


pointDecoder : Point -> Foobar
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
    = ToPlot PlotNr (PlotMsg Foobar)
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
            ( { model | plot1 = plotUpdate plotMsg model.plot1 }
            , Cmd.none
            )

        ToPlot Plot2 plotMsg ->
            ( { model | plot2 = plotUpdate plotMsg model.plot2 }
            , Cmd.none
            )

        ToPlot Plot3 plotMsg ->
            ( { model | plot3 = plotUpdate plotMsg model.plot3 }
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
-- myLine : Colors.Color -> Dots.Shape -> String -> List Foobar -> Line


myLine color shape title data =
    LineChart.line color shape title data



-- myLine Colors.blueLight Dots.triangle "exp" data1


lines1 : Lines Foobar
lines1 =
    [ myLine Colors.goldLight Dots.diamond "polysin" polySinData
    , myLine Colors.greenLight Dots.square "poly" polyData
    ]


lines2 : Lines Foobar
lines2 =
    [ myLine Colors.greenLight Dots.square "poly" polyData
    , myLine Colors.goldLight Dots.diamond "polysin" polySinData
    , myLine Colors.pinkLight Dots.plus "poly3xsin" poly3xSinData
    ]


lines3 : Lines Foobar
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
