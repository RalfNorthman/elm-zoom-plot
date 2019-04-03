module Plot
    exposing
        ( PlotState
        , plotInit
        , PlotMsg(..)
        , PlotConfig
        , plotUpdate
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
import Html.Attributes exposing (style)
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
import TypedSvg.Types exposing (..)
import TypedSvg.Core
import TypedSvg


format : Float -> String
format number =
    NumberSuffix.format
        { scientificConfig | locale = frenchLocale }
        number


type alias Lines data =
    List (Series data)


type alias PlotConfig data =
    { lines : Lines data
    , xIsTime : Bool
    , xAcc : data -> Float
    , yAcc : data -> Float
    , pointDecoder : Point -> data
    , labelFunc : data -> String
    , xLabel : String
    , yLabel : String
    }


draw :
    Float
    -> Float
    -> PlotState data
    -> (PlotMsg data -> msg)
    -> Element msg
draw width height plotState toMsg =
    Element.map toMsg
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
         <|
            Element.html
                (chart plotState width height)
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
            , grid = True
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
            posixToTimeWithSeconds time
                ++ sweFormat
                    [ Format.text "."
                    , Format.millisecondFixed
                    ]
                    time

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
            posixToMonthNameYear time

        ( time, Tick.Year ) ->
            sweFormat
                [ Format.yearNumber ]
                time


customFormat : Tick.Time -> String
customFormat info =
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            sweFormat
                [ Format.text "."
                , Format.millisecondFixed
                ]
                time

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
            sweFormat
                [ Format.monthNameAbbreviated ]
                time

        ( time, Tick.Year ) ->
            sweFormat
                [ Format.yearNumber ]
                time


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
            posixToMonthNameYear time

        ( time, Tick.Year ) ->
            "huh"



---- CHART ----


eventsConfig : PlotState data -> Events.Config data (PlotMsg data)
eventsConfig state =
    let
        myGetData : Events.Decoder data data
        myGetData =
            Events.map state.config.pointDecoder Events.getData
    in
        Events.custom
            [ Events.onMouseDown MouseDown myGetData
            , Events.onMouseUp MouseUp myGetData
            , case state.mouseDown of
                Nothing ->
                    Events.onMouseMove Hover Events.getNearest

                Just _ ->
                    Events.onMouseMove Move myGetData
            , Events.onMouseLeave MouseLeave
            ]


xAxisConfig : PlotState data -> Float -> Axis.Config data msg
xAxisConfig state width =
    Axis.custom
        { title = Title.default state.config.xLabel
        , variable = Just << state.config.xAcc
        , pixels = round width
        , range = setRange state.xZoom
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = xTicksConfig state.config.xIsTime state.xZoom
        }


yAxisConfig : PlotState data -> Float -> Axis.Config data msg
yAxisConfig state height =
    Axis.custom
        { title = Title.atAxisMax 70 0 state.config.yLabel
        , variable = Just << state.config.yAcc
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
            , grid = True
            , direction = Tick.negative
            , label = Just label
            }


fontSize : Float
fontSize =
    12


containerConfig : Container.Config msg
containerConfig =
    Container.custom
        { attributesHtml = []
        , attributesSvg = [ SvgAttrPx.fontSize fontSize ]
        , size = Container.relative
        , margin =
            { top = 30
            , right = 110
            , bottom = 30
            , left = 50
            }
        , id = "whatever"
        }


dragBox : PlotState data -> data -> data -> Coordinate.System -> Svg msg
dragBox state a b system =
    let
        xAcc =
            state.config.xAcc

        yAcc =
            state.config.yAcc
    in
        Junk.rectangle system
            [ SvgAttr.fill <| Fill Colors.grayLightest
            , SvgAttr.stroke Colors.grayLight
            , SvgAttrPx.strokeWidth 1
            , SvgAttr.strokeDasharray "3 3"
            ]
            (min (xAcc a) (xAcc b))
            (max (xAcc a) (xAcc b))
            (min (yAcc a) (yAcc b))
            (max (yAcc a) (yAcc b))


type Hover
    = RawX
    | RawY


hoverJunk :
    PlotState data
    -> data
    -> Coordinate.System
    -> Svg (PlotMsg data)
hoverJunk state hovered sys =
    let
        xAcc =
            state.config.xAcc

        yAcc =
            state.config.yAcc

        xIsTime =
            state.config.xIsTime

        customLabel =
            state.config.labelFunc hovered

        textDate =
            if xIsTime then
                TimeHelpers.posixToDate
                    (Time.millisToPosix <| round (xAcc hovered))
            else
                ""

        textX =
            if xIsTime then
                TimeHelpers.posixToTimeWithSeconds
                    (Time.millisToPosix <| round (xAcc hovered))
            else
                format (xAcc hovered)

        textY =
            format (yAcc hovered)

        mySvgText : Float -> String -> Svg (PlotMsg data)
        mySvgText fontHeights str =
            TypedSvg.text_
                [ SvgAttrPx.dy <| fontHeights * fontSize
                , SvgAttr.alignmentBaseline AlignmentTextBeforeEdge
                , SvgAttr.textAnchor AnchorEnd
                ]
                [ TypedSvg.Core.text str ]

        svgList =
            [ TypedSvg.g [ style "pointer-events" "none" ]
                [ mySvgText -3 customLabel
                , mySvgText -2 textDate
                , mySvgText -1 textX
                , mySvgText 0 textY
                ]
            ]
    in
        Junk.placed
            sys
            (xAcc hovered)
            (yAcc hovered)
            60
            -15
            svgList


junkConfig : PlotState data -> Junk.Config data (PlotMsg data)
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
                            , above =
                                [ hoverJunk
                                    state
                                    hovered
                                    sys
                                ]
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
                            { below =
                                [ dragBox
                                    state
                                    downPoint
                                    movedPoint
                                    sys
                                ]
                            , above = []
                            , html = []
                            }
                        )


dotsConfig : Maybe data -> Dots.Config data
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


chartConfig :
    PlotState data
    -> Float
    -> Float
    -> Config data (PlotMsg data)
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


chart : PlotState data -> Float -> Float -> Svg (PlotMsg data)
chart state width height =
    viewCustom
        (chartConfig state width height)
        state.config.lines


type alias PlotState data =
    { mouseDown : Maybe data
    , rangeX : Maybe Range
    , rangeY : Maybe Range
    , xZoom : Zoom
    , yZoom : Zoom
    , hovered : Maybe data
    , moved : Maybe data
    , movedSinceMouseDown : Int
    , config : PlotConfig data
    }


type PlotMsg data
    = MouseDown data
    | MouseUp data
    | Hover (Maybe data)
    | Move data
    | MouseLeave
    | UpdateConfig (PlotConfig data)


plotInit : PlotConfig data -> PlotState data
plotInit config =
    { mouseDown = Nothing
    , rangeX = Nothing
    , rangeY = Nothing
    , xZoom = UnZoomed
    , yZoom = UnZoomed
    , hovered = Nothing
    , moved = Nothing
    , movedSinceMouseDown = 0
    , config = config
    }


type XY
    = X
    | Y


newRange : PlotState data -> data -> XY -> ( Maybe Range, Zoom )
newRange state mouseUp xy =
    case state.mouseDown of
        Just a ->
            let
                b =
                    mouseUp

                acc =
                    case xy of
                        X ->
                            state.config.xAcc

                        Y ->
                            state.config.yAcc

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


zoomUpdate : PlotState data -> data -> PlotState data
zoomUpdate state point =
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


plotUpdate : PlotMsg data -> PlotState data -> PlotState data
plotUpdate msg state =
    case msg of
        MouseDown point ->
            case state.mouseDown of
                Nothing ->
                    { state
                        | mouseDown = Just point
                        , hovered = Nothing
                        , moved = Nothing
                    }

                Just oldPoint ->
                    zoomUpdate state point

        MouseUp point ->
            case state.mouseDown of
                Just _ ->
                    zoomUpdate state point

                Nothing ->
                    state

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
            { state | hovered = Nothing }

        UpdateConfig newConfig ->
            { state | config = newConfig }
