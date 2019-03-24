module Main exposing (..)

import Browser
import Browser.Events
import Browser.Dom exposing (Viewport, Error, getViewportOf)
import Task exposing (Task)
import Debug
import NumberSuffix exposing (scientificConfig)
import FormatNumber.Locales exposing (frenchLocale)
import Html exposing (Html)
import Html.Attributes
import Element exposing (..)
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


xAxisConfig : PlotState -> Axis.Config DataPoint msg
xAxisConfig state =
    Axis.custom
        { title = Title.default "x"
        , variable = Just << .x
        , pixels = round state.plotWidth
        , range = state.xConfig
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = ticksConfig
        }


yAxisConfig : PlotState -> Axis.Config DataPoint msg
yAxisConfig state =
    Axis.custom
        { title = Title.default "y"
        , variable = Just << .y
        , pixels = round state.plotHeight
        , range = state.yConfig
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = ticksConfig
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
        , margin = Container.Margin 40 110 50 70
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


chartConfig : PlotState -> Config DataPoint PlotMsg
chartConfig state =
    { x = xAxisConfig state
    , y = yAxisConfig state
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


chart : PlotState -> Lines -> Html PlotMsg
chart state lines =
    viewCustom
        (chartConfig state)
        lines



---- MODEL ----


type alias Model =
    { plot1 : PlotState
    , plot2 : PlotState
    }


type alias PlotState =
    { mouseDown : Maybe DataPoint
    , rangeX : Maybe Range
    , rangeY : Maybe Range
    , xConfig : Range.Config
    , yConfig : Range.Config
    , hovered : Maybe DataPoint
    , moved : Maybe DataPoint
    , plotWidth : Float
    , plotHeight : Float
    , id : Id
    , nr : PlotNr
    , movedSinceMouseDown : Int
    }


unZoomed : Range.Config
unZoomed =
    Range.padded 20 20


plotInit : Id -> PlotNr -> PlotState
plotInit plotId nr =
    { mouseDown = Nothing
    , rangeX = Nothing
    , rangeY = Nothing
    , xConfig = unZoomed
    , yConfig = unZoomed
    , hovered = Nothing
    , moved = Nothing
    , plotWidth = 0
    , plotHeight = 0
    , id = plotId
    , nr = nr
    , movedSinceMouseDown = 0
    }


init : ( Model, Cmd Msg )
init =
    ( { plot1 = plotInit id1 Plot1
      , plot2 = plotInit id2 Plot2
      }
    , getDims
    )


getDims : Cmd Msg
getDims =
    Task.sequence
        [ taskGetDims id1
        , taskGetDims id2
        ]
        |> Task.attempt NewDims



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


type Msg
    = Plot PlotNr PlotMsg
    | Resize Int Int
    | NewDims (Result Error (List Viewport))


type PlotMsg
    = MouseDown DataPoint
    | MouseUp DataPoint
    | Hover (Maybe DataPoint)
    | Move DataPoint
    | MouseLeave
    | Dims Viewport


taskGetDims : Id -> Task Error Viewport
taskGetDims plotId =
    getViewportOf plotId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Plot Plot1 plotMsg ->
            ( { model | plot1 = plotUpdate plotMsg model.plot1 }
            , Cmd.none
            )

        Plot Plot2 plotMsg ->
            ( { model | plot2 = plotUpdate plotMsg model.plot2 }
            , Cmd.none
            )

        Resize _ _ ->
            ( model, getDims )

        NewDims result ->
            case result of
                Ok [ dims1, dims2 ] ->
                    ( { model
                        | plot1 =
                            plotUpdate
                                (Dims dims1)
                                model.plot1
                        , plot2 =
                            plotUpdate
                                (Dims dims2)
                                model.plot2
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


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

        Dims { viewport } ->
            { state
                | plotWidth = viewport.width
                , plotHeight = viewport.height
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


id : String -> Attribute msg
id str =
    htmlAttribute <| Html.Attributes.id str


id1 : String
id1 =
    "plot1"


id2 : String
id2 =
    "plot2"


plot : PlotNr -> PlotState -> Id -> Lines -> Element Msg
plot nr state plotId lines =
    Element.map (\msg -> Plot nr msg)
        (el
            [ width fill
            , height fill
            , id plotId
            ]
         <|
            html (chart state lines)
        )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , padding 10
        ]
    <|
        row
            [ width fill
            , height fill
            , padding 5
            ]
            [ plot Plot1 model.plot1 id1 lines1
            , plot Plot2 model.plot2 id2 lines2
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
