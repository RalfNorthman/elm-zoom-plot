module Main exposing (..)

import Color exposing (toRgba)
import Browser
import Browser.Events
import Browser.Dom exposing (Viewport, Error, getViewport)
import Task exposing (Task)
import Debug
import NumberSuffix exposing (scientificConfig)
import FormatNumber.Locales exposing (frenchLocale)
import Html exposing (Html)
import Html.Attributes
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import LineChart exposing (..)
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import LineChart.Coordinate as Coordinate exposing (Range)
import LineChart.Axis.Title as Title
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Tick as Tick
import Svg exposing (Svg)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as SvgAttrPx
import TypedSvg.Types exposing (Fill(..))


---- TEST-DATA ----


type alias DataPoint =
    { x : Float, y : Float }


makeData : (Float -> Float) -> List DataPoint
makeData func =
    let
        xs : List Float
        xs =
            List.map
                (\x -> toFloat x * 0.1)
            <|
                List.range -221 65
    in
        List.map2 DataPoint xs <|
            List.map func xs


data1 : List DataPoint
data1 =
    makeData (\x -> 0.01 ^ (0.04 * x) + 3.5 * (sin (2 * x)))


data2 : List DataPoint
data2 =
    makeData cos


data3 : List DataPoint
data3 =
    makeData (\x -> 0.01 * x * x - 0.1 * x - 0.5)


data4 : List DataPoint
data4 =
    makeData (\x -> 0.01 * x ^ 2 - 0.1 * x - 0.5 + sin x)


data5 : List DataPoint
data5 =
    makeData
        (\x -> 0.03 * x ^ 2 - 0.5 * x - 3.5 + 5 * sin (3 * x))


format : Float -> String
format number =
    NumberSuffix.format
        { scientificConfig | locale = frenchLocale }
        number



---- CHART ----


eventsConfig : PlotState -> Events.Config DataPoint PlotMsg
eventsConfig state =
    Events.custom
        [ Events.onMouseDown MouseDown Events.getData
        , Events.onMouseUp MouseUp Events.getData
        , case state.mouseDown of
            Nothing ->
                Events.onMouseMove Hover Events.getNearest

            Just _ ->
                Events.onMouseMove Move Events.getData
        , Events.onMouseLeave MouseLeave
        ]


xAxisConfig : PlotState -> Float -> Axis.Config DataPoint msg
xAxisConfig state width =
    Axis.custom
        { title = Title.default "x"
        , variable = Just << .x
        , pixels = round width
        , range = state.xConfig
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = ticksConfig
        }


yAxisConfig : PlotState -> Float -> Axis.Config DataPoint msg
yAxisConfig state height =
    Axis.custom
        { title = Title.default "y"
        , variable = Just << .y
        , pixels = round height
        , range = state.yConfig
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = Ticks.floatCustom 4 customTick
        }


ticksConfig : Ticks.Config msg
ticksConfig =
    Ticks.floatCustom 7 customTick


customTick : Float -> Tick.Config msg
customTick value =
    let
        label =
            Junk.label Colors.black (format value)
    in
        Tick.custom
            { position = value
            , color = Colors.black
            , width = 1
            , length = 2
            , grid = False
            , direction = Tick.negative
            , label = Just label
            }


containerConfig : Container.Config msg
containerConfig =
    Container.custom
        { attributesHtml = []
        , attributesSvg = [ SvgAttrPx.fontSize 10 ]
        , size = Container.relative
        , margin =
            { top = 30
            , right = 110
            , bottom = 30
            , left = 50
            }
        , id = "whatever"
        }


dragBox : DataPoint -> DataPoint -> Coordinate.System -> Svg msg
dragBox a b system =
    Junk.rectangle system
        [ SvgAttr.fill <| Fill Colors.grayLightest
        , SvgAttr.stroke Colors.grayLight
        , SvgAttrPx.strokeWidth 1
        , SvgAttr.strokeDasharray "3 3"
        ]
        (min a.x b.x)
        (max a.x b.x)
        (min a.y b.y)
        (max a.y b.y)


hoverJunk : DataPoint -> Coordinate.System -> List (Svg msg)
hoverJunk hovered system =
    let
        textX =
            format hovered.x

        textY =
            format hovered.y

        label sys offsetY text =
            Junk.labelAt
                sys
                hovered.x
                hovered.y
                8
                offsetY
                "anchor-blah"
                Colors.black
                text
    in
        [ label system -15 textX
        , label system -5 textY
        ]


junkConfig : PlotState -> Junk.Config DataPoint msg
junkConfig state =
    case state.mouseDown of
        Nothing ->
            case state.hovered of
                Nothing ->
                    Junk.default

                Just hovered ->
                    Junk.custom
                        (\sys ->
                            { below = []
                            , above = hoverJunk hovered sys
                            , html = []
                            }
                        )

        Just downPoint ->
            case state.moved of
                Nothing ->
                    Junk.default

                Just movedPoint ->
                    Junk.custom
                        (\sys ->
                            { below = [ dragBox downPoint movedPoint sys ]
                            , above = []
                            , html = []
                            }
                        )


dotsConfig : Maybe DataPoint -> Dots.Config DataPoint
dotsConfig hovered =
    let
        noDot =
            Dots.full 0

        dot =
            Dots.disconnected 10 1

        styleLegend _ =
            dot

        styleIndividual point =
            case hovered of
                Just hoveredPoint ->
                    if point == hoveredPoint then
                        dot
                    else
                        noDot

                Nothing ->
                    noDot
    in
        Dots.customAny
            { legend = styleLegend
            , individual = styleIndividual
            }


chartConfig : PlotState -> Float -> Float -> Config DataPoint PlotMsg
chartConfig state width height =
    { x = xAxisConfig state width
    , y = yAxisConfig state height
    , container = containerConfig
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.default
    , events = eventsConfig state
    , junk = junkConfig state
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = dotsConfig state.hovered
    }


chart : PlotState -> Float -> Float -> Lines -> Html PlotMsg
chart state width height lines =
    viewCustom
        (chartConfig state width height)
        lines



---- MODEL ----


type alias Model =
    { plot1 : PlotState
    , plot2 : PlotState
    , plot3 : PlotState
    , plotWidth : Float
    , plotHeight : Float
    }


type alias PlotState =
    { mouseDown : Maybe DataPoint
    , rangeX : Maybe Range
    , rangeY : Maybe Range
    , xConfig : Range.Config
    , yConfig : Range.Config
    , hovered : Maybe DataPoint
    , moved : Maybe DataPoint
    , nr : PlotNr
    , movedSinceMouseDown : Int
    }


unZoomed : Range.Config
unZoomed =
    Range.padded 20 20


plotInit : PlotNr -> PlotState
plotInit nr =
    { mouseDown = Nothing
    , rangeX = Nothing
    , rangeY = Nothing
    , xConfig = unZoomed
    , yConfig = unZoomed
    , hovered = Nothing
    , moved = Nothing
    , nr = nr
    , movedSinceMouseDown = 0
    }


init : ( Model, Cmd Msg )
init =
    ( { plot1 = plotInit Plot1
      , plot2 = plotInit Plot2
      , plot3 = plotInit Plot3
      , plotWidth = 0
      , plotHeight = 0
      }
    , getBrowserSize
    )



---- UPDATE ----


type XY
    = X
    | Y


newRange : PlotState -> DataPoint -> XY -> ( Maybe Range, Range.Config )
newRange state mouseUp xy =
    case state.mouseDown of
        Just a ->
            let
                b =
                    mouseUp

                acc =
                    case xy of
                        X ->
                            .x

                        Y ->
                            .y

                zoomMin =
                    min (acc a) (acc b)

                zoomMax =
                    max (acc a) (acc b)
            in
                if state.movedSinceMouseDown > 2 then
                    ( Just <| Range zoomMin zoomMax
                    , Range.window zoomMin zoomMax
                    )
                else
                    ( Nothing, unZoomed )

        Nothing ->
            ( Nothing, unZoomed )


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


type PlotMsg
    = MouseDown DataPoint
    | MouseUp DataPoint
    | Hover (Maybe DataPoint)
    | Move DataPoint
    | MouseLeave


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


plotUpdate : PlotMsg -> PlotState -> PlotState
plotUpdate msg state =
    case msg of
        MouseDown point ->
            { state
                | mouseDown = Just point
                , hovered = Nothing
                , moved = Nothing
            }

        MouseUp point ->
            let
                ( rx, xc ) =
                    newRange state point X

                ( ry, yc ) =
                    newRange state point Y
            in
                { state
                    | xConfig = xc
                    , yConfig = yc
                    , rangeX = rx
                    , rangeY = ry
                    , mouseDown = Nothing
                    , movedSinceMouseDown = 0
                }

        Hover point ->
            { state | hovered = point }

        Move point ->
            case state.mouseDown of
                Nothing ->
                    { state | moved = Just point }

                Just _ ->
                    let
                        oldCount =
                            state.movedSinceMouseDown
                    in
                        { state
                            | moved = Just point
                            , movedSinceMouseDown = oldCount + 1
                        }

        MouseLeave ->
            { state
                | hovered = Nothing
                , mouseDown = Nothing
                , movedSinceMouseDown = 0
            }



---- VIEW ----


type alias Lines =
    List (Series DataPoint)


lines1 : Lines
lines1 =
    [ LineChart.line Colors.blueLight Dots.triangle "exp" data1
    , LineChart.line Colors.pinkLight Dots.plus "poly3xsin" data5
    ]


lines2 : Lines
lines2 =
    [ LineChart.line Colors.tealLight Dots.circle "cos" data2
    , LineChart.line Colors.greenLight Dots.square "poly" data3
    , LineChart.line Colors.goldLight Dots.diamond "polysin" data4
    ]


lines3 : Lines
lines3 =
    [ LineChart.line Colors.tealLight Dots.circle "cos" data2
    , LineChart.line Colors.goldLight Dots.diamond "polysin" data4
    , LineChart.line Colors.pinkLight Dots.plus "poly3xsin" data5
    ]


id : String -> Attribute msg
id str =
    htmlAttribute <| Html.Attributes.id str


type alias Plot =
    { nr : PlotNr
    , acc : Model -> PlotState
    , lines : Lines
    }


plot1 : Plot
plot1 =
    Plot Plot1 .plot1 lines1


plot2 : Plot
plot2 =
    Plot Plot2 .plot2 lines2


plot3 : Plot
plot3 =
    Plot Plot3 .plot3 lines3


draw : Model -> Plot -> Element Msg
draw model plot =
    Element.map (\msg -> ToPlot plot.nr msg)
        (el
            [ width fill
            , height fill
            ]
         <|
            html
                (chart (plot.acc model)
                    model.plotWidth
                    model.plotHeight
                    plot.lines
                )
        )


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
                    , id "plotColumn1"
                    ]
                    [ draw model plot1
                    , draw model plot2
                    , draw model plot3
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
