module Plot
    exposing
        ( PlotState
        , plotInit
        , PlotMsg
        , PlotConfig
        , plotUpdate
        , toPoints
        , draw
        , Lines
        )

import DateFormat as Format
import DateFormat.Language exposing (swedish)
import NumberSuffix exposing (format, scientificConfig)
import FormatNumber.Locales exposing (frenchLocale)
import TimeHelpers exposing (..)
import Time exposing (Posix)
import Html exposing (Html)
import NumberSuffix exposing (scientificConfig)
import FormatNumber.Locales exposing (frenchLocale)
import Element exposing (Element)
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
import LineChart.Coordinate as Coordinate exposing (Range, Point)
import LineChart.Axis.Title as Title
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Values as Values
import Svg exposing (Svg)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as SvgAttrPx
import TypedSvg.Types exposing (Fill(..))


format : Float -> String
format number =
    NumberSuffix.format
        { scientificConfig | locale = frenchLocale }
        number


type alias Lines =
    List (Series Point)


toPoints : (data -> Float) -> (data -> Float) -> List data -> List Point
toPoints xAcc yAcc records =
    let
        constructor =
            (\record -> Point (xAcc record) (yAcc record))
    in
        List.map constructor records


type alias PlotConfig =
    { plotState : PlotState
    , lines : Lines
    , width : Float
    , height : Float
    , xIsTime : Bool
    }


draw : PlotConfig -> (PlotMsg -> msg) -> Element msg
draw plot toMsg =
    Element.map toMsg
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
         <|
            Element.html
                (chart
                    plot.plotState
                    plot.width
                    plot.height
                    plot.lines
                    plot.xIsTime
                )
        )


timeConvert : Posix -> Float
timeConvert =
    (Time.posixToMillis >> toFloat)



---- TIME PLOTTING STUFF ----


customTimeTick : Tick.Time -> Tick.Config msg
customTimeTick info =
    let
        label =
            customFormatRouter info
    in
        Tick.custom
            { position = toFloat (Time.posixToMillis info.timestamp)
            , color = Colors.gray
            , width = 1
            , length = 8
            , grid = False
            , direction = Tick.negative
            , label = Just <| Junk.label Colors.black label
            }


customFormatRouter : Tick.Time -> String
customFormatRouter info =
    case ( info.isFirst, info.change ) of
        ( True, _ ) ->
            customFormatFirst info

        ( _, Just _ ) ->
            customFormatChange info

        _ ->
            customFormat info


customFormatFirst : Tick.Time -> String
customFormatFirst info =
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            ""

        ( time, Tick.Second ) ->
            posixToTimeWithSeconds time

        ( time, Tick.Minute ) ->
            posixToTime time

        ( time, Tick.Hour ) ->
            posixToNameDate time
                ++ " "
                ++ posixToTime time

        ( time, Tick.Day ) ->
            posixToNameDate time

        ( time, Tick.Month ) ->
            "first y!"

        ( time, Tick.Year ) ->
            "huh"


customFormat : Tick.Time -> String
customFormat info =
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            ""

        ( time, Tick.Second ) ->
            sweFormat
                [ Format.text ":"
                , Format.secondFixed
                ]
                time

        ( time, Tick.Minute ) ->
            sweFormat
                [ Format.text ":"
                , Format.minuteFixed
                ]
                time

        ( time, Tick.Hour ) ->
            posixToTime time

        ( time, Tick.Day ) ->
            posixToNameDate time

        ( time, Tick.Month ) ->
            "m"

        ( time, Tick.Year ) ->
            "y"


customFormatChange : Tick.Time -> String
customFormatChange info =
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            posixToTimeWithSeconds time

        ( time, Tick.Second ) ->
            posixToTime time

        ( time, Tick.Minute ) ->
            posixToTime time

        ( time, Tick.Hour ) ->
            posixToNameDate time

        ( time, Tick.Day ) ->
            posixToNameDate time

        ( time, Tick.Month ) ->
            "new y!"

        ( time, Tick.Year ) ->
            "huh"



---- CHART ----


eventsConfig : PlotState -> Events.Config Point PlotMsg
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


xAxisConfig : PlotState -> Float -> Bool -> Axis.Config Point msg
xAxisConfig state width xIsTime =
    Axis.custom
        { title = Title.default "x"
        , variable = Just << .x
        , pixels = round width
        , range = setRange state.xZoom
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = xTicksConfig xIsTime state.xZoom
        }


yAxisConfig : PlotState -> Float -> Axis.Config Point msg
yAxisConfig state height =
    Axis.custom
        { title = Title.default "y"
        , variable = Just << .y
        , pixels = round height
        , range = setRange state.yZoom
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = Ticks.floatCustom 4 customTick
        }


type Zoom
    = UnZoomed
    | Zoomed Float Float


setRange : Zoom -> Range.Config
setRange zoom =
    case zoom of
        UnZoomed ->
            Range.padded 20 20

        Zoomed min max ->
            Range.window min max


valuesWithin : Coordinate.Range -> List Tick.Time
valuesWithin =
    Values.time Time.utc 5


xTicksConfig : Bool -> Zoom -> Ticks.Config msg
xTicksConfig xIsTime zoom =
    if xIsTime then
        Ticks.custom <|
            \dataRange axisRange ->
                let
                    range =
                        case zoom of
                            UnZoomed ->
                                dataRange

                            Zoomed _ _ ->
                                axisRange
                in
                    List.map customTimeTick (valuesWithin range)
    else
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


dragBox : Point -> Point -> Coordinate.System -> Svg msg
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


hoverJunk : Point -> Coordinate.System -> Bool -> List (Svg msg)
hoverJunk hovered system xIsTime =
    let
        textDate =
            if xIsTime then
                TimeHelpers.posixToDate
                    (Time.millisToPosix <| round hovered.x)
            else
                ""

        textX =
            if xIsTime then
                TimeHelpers.posixToTimeWithSeconds
                    (Time.millisToPosix <| round hovered.x)
            else
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
        [ label system -25 textDate
        , label system -15 textX
        , label system -5 textY
        ]


junkConfig : PlotState -> Bool -> Junk.Config Point msg
junkConfig state xIsTime =
    case state.mouseDown of
        Nothing ->
            case state.hovered of
                Nothing ->
                    Junk.default

                Just hovered ->
                    Junk.custom
                        (\sys ->
                            { below = []
                            , above = hoverJunk hovered sys xIsTime
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


dotsConfig : Maybe Point -> Dots.Config Point
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


chartConfig : PlotState -> Float -> Float -> Bool -> Config Point PlotMsg
chartConfig state width height xIsTime =
    { x = xAxisConfig state width xIsTime
    , y = yAxisConfig state height
    , container = containerConfig
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.default
    , events = eventsConfig state
    , junk = junkConfig state xIsTime
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = dotsConfig state.hovered
    }


chart : PlotState -> Float -> Float -> Lines -> Bool -> Svg PlotMsg
chart state width height lines xIsTime =
    viewCustom
        (chartConfig state width height xIsTime)
        lines


type alias PlotState =
    { mouseDown : Maybe Point
    , rangeX : Maybe Range
    , rangeY : Maybe Range
    , xZoom : Zoom
    , yZoom : Zoom
    , hovered : Maybe Point
    , moved : Maybe Point
    , movedSinceMouseDown : Int
    }


type PlotMsg
    = MouseDown Point
    | MouseUp Point
    | Hover (Maybe Point)
    | Move Point
    | MouseLeave


plotInit : PlotState
plotInit =
    { mouseDown = Nothing
    , rangeX = Nothing
    , rangeY = Nothing
    , xZoom = UnZoomed
    , yZoom = UnZoomed
    , hovered = Nothing
    , moved = Nothing
    , movedSinceMouseDown = 0
    }


type XY
    = X
    | Y


newRange : PlotState -> Point -> XY -> ( Maybe Range, Zoom )
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
                    , Zoomed zoomMin zoomMax
                    )
                else
                    ( Nothing, UnZoomed )

        Nothing ->
            ( Nothing, UnZoomed )


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
                    | xZoom = xc
                    , yZoom = yc
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
