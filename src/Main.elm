module Main exposing (..)

import Plot exposing (..)
import Color exposing (toRgba)
import Browser
import Browser.Events
import Browser.Dom exposing (Viewport, Error, getViewport)
import Task exposing (Task)
import Debug
import Html exposing (Html)
import Html.Attributes
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import LineChart.Colors as Colors
import LineChart.Dots as Dots
import LineChart


---- TEST-DATA ----


type alias Foobar =
    { foo : Float
    , bar : Float
    , baz : String
    }


type alias Point =
    { x : Float, y : Float }


makeData : (Float -> Float) -> List Foobar
makeData func =
    let
        xs : List Float
        xs =
            List.map
                (\x -> 101001000 + toFloat x * 2000000)
            <|
                List.range -221 65
    in
        List.map (\x -> Foobar x (func x) "foobar") xs


data1 : List Foobar
data1 =
    makeData (\x -> 0.01 ^ (0.04 * x) + 3.5 * (sin (2 * x)))


data2 : List Foobar
data2 =
    makeData cos


data3 : List Foobar
data3 =
    makeData (\x -> 0.01 * x * x - 0.1 * x - 0.5)


data4 : List Foobar
data4 =
    makeData (\x -> 0.01 * x ^ 2 - 0.1 * x - 0.5 + sin x)


data5 : List Foobar
data5 =
    makeData
        (\x -> 0.03 * x ^ 2 - 0.5 * x - 3.5 + 5 * sin (3 * x))



---- MODEL ----


type alias Model =
    { plot1 : PlotState
    , plot2 : PlotState
    , plot3 : PlotState
    , plotWidth : Float
    , plotHeight : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { plot1 = plotInit
      , plot2 = plotInit
      , plot3 = plotInit
      , plotWidth = 0
      , plotHeight = 0
      }
    , getBrowserSize
    )



---- UPDATE ----


type alias Id =
    String


type PlotNr
    = Plot1
    | Plot2
    | Plot3


type Msg
    = ToPlot PlotNr PlotMsg
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
    LineChart.line color shape title (toPoints .foo .bar data)



-- myLine Colors.blueLight Dots.triangle "exp" data1


lines1 : Lines
lines1 =
    [ myLine Colors.pinkLight Dots.plus "poly3xsin" data5
    , myLine Colors.greenLight Dots.square "poly" data3
    ]


lines2 : Lines
lines2 =
    [ myLine Colors.tealLight Dots.circle "cos" data2
    , myLine Colors.greenLight Dots.square "poly" data3
    , myLine Colors.goldLight Dots.diamond "polysin" data4
    ]


lines3 : Lines
lines3 =
    [ myLine Colors.tealLight Dots.circle "cos" data2
    , myLine Colors.goldLight Dots.diamond "polysin" data4
    , myLine Colors.pinkLight Dots.plus "poly3xsin" data5
    ]


plot1 : Model -> PlotConfig
plot1 model =
    PlotConfig model.plot1 lines1 model.plotWidth model.plotHeight False


plot2 : Model -> PlotConfig
plot2 model =
    PlotConfig model.plot2 lines2 model.plotWidth model.plotHeight True


plot3 : Model -> PlotConfig
plot3 model =
    PlotConfig model.plot3 lines3 model.plotWidth model.plotHeight False


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
                    [ draw (plot1 model) (\msg -> ToPlot Plot1 msg)
                    , draw (plot2 model) (\msg -> ToPlot Plot2 msg)
                    , draw (plot3 model) (\msg -> ToPlot Plot3 msg)
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
