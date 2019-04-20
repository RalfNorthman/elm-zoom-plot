module Plot exposing
    ( Config
    , Msg
    , State
    , defaultConfigWith
    , draw
    , drawHtml
    , init
    , pointDefaultConfig
    , update
    )

import Color
import DateFormat as Format
import DateFormat.Language exposing (swedish)
import Element exposing (Element)
import FormatNumber.Locales exposing (frenchLocale)
import Html exposing (Html)
import Html.Attributes exposing (style)
import LineChart exposing (..)
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Axis.Values as Values
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate exposing (Point, Range)
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import NumberSuffix exposing (format, scientificConfig)
import Svg exposing (Svg)
import Time exposing (Posix)
import TimeHelpers exposing (..)
import TypedSvg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as SvgAttrPx
import TypedSvg.Core
import TypedSvg.Types exposing (..)


format : Float -> String
format number =
    NumberSuffix.format
        { scientificConfig | locale = frenchLocale }
        number


type alias Config data =
    { lines : List (Series data)
    , xIsTime : Bool
    , showLegends : Bool
    , xAcc : data -> Float
    , yAcc : data -> Float
    , pointDecoder : Point -> data
    , labelFunc : data -> String
    , xAxisLabel : String
    , yAxisLabel : String
    , margin :
        { top : Float
        , right : Float
        , bottom : Float
        , left : Float
        }
    , xAxisLabelOffsetX : Float
    , xAxisLabelOffsetY : Float
    , yAxisLabelOffsetX : Float
    , yAxisLabelOffsetY : Float
    }


defaultConfigWith :
    List data
    -> (data -> Float)
    -> (data -> Float)
    -> (Point -> data)
    -> Config data
defaultConfigWith dataList xAcc yAcc pointDecoder =
    { lines = [ LineChart.line Colors.tealLight Dots.circle "" dataList ]
    , xIsTime = False
    , showLegends = False
    , xAcc = xAcc
    , yAcc = yAcc
    , pointDecoder = pointDecoder
    , labelFunc = \_ -> ""
    , xAxisLabel = ""
    , yAxisLabel = ""
    , margin =
        { top = 20
        , right = 30
        , bottom = 30
        , left = 60
        }
    , xAxisLabelOffsetX = 0
    , xAxisLabelOffsetY = 0
    , yAxisLabelOffsetX = 0
    , yAxisLabelOffsetY = 0
    }


pointDefaultConfig : List Point -> Config Point
pointDefaultConfig points =
    { lines = [ LineChart.line Colors.tealLight Dots.circle "" points ]
    , xIsTime = False
    , showLegends = False
    , xAcc = .x
    , yAcc = .y
    , pointDecoder = \p -> p
    , labelFunc = \_ -> ""
    , xAxisLabel = ""
    , yAxisLabel = ""
    , margin =
        { top = 20
        , right = 30
        , bottom = 30
        , left = 60
        }
    , xAxisLabelOffsetX = 0
    , xAxisLabelOffsetY = 0
    , yAxisLabelOffsetX = 0
    , yAxisLabelOffsetY = 0
    }


draw :
    Float
    -> Float
    -> Config data
    -> State data
    -> (Msg data -> msg)
    -> Element msg
draw width height config state toMsg =
    Element.map toMsg
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
         <|
            Element.html
                (chart width height config state)
        )


drawHtml :
    Float
    -> Float
    -> Config data
    -> State data
    -> (Msg data -> msg)
    -> Html msg
drawHtml width height config state toMsg =
    Element.layout [] <| draw width height config state toMsg



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


eventsConfig : Config data -> State data -> Events.Config data (Msg data)
eventsConfig config state =
    let
        myGetData : Events.Decoder data data
        myGetData =
            Events.map config.pointDecoder Events.getData
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


xAxisConfig : Float -> Config data -> State data -> Axis.Config data msg
xAxisConfig width config state =
    Axis.custom
        { title =
            Title.atAxisMax
                config.xAxisLabelOffsetX
                config.xAxisLabelOffsetY
                config.xAxisLabel
        , variable = Just << config.xAcc
        , pixels = round width
        , range = setRange state.xZoom
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = xTicksConfig config.xIsTime state.xZoom
        }


yAxisConfig : Float -> Config data -> State data -> Axis.Config data msg
yAxisConfig height config state =
    Axis.custom
        { title =
            Title.atAxisMax
                config.yAxisLabelOffsetX
                config.yAxisLabelOffsetY
                config.yAxisLabel
        , variable = Just << config.yAcc
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
    14


containerConfig : Config data -> Container.Config msg
containerConfig config =
    Container.custom
        { attributesHtml = []
        , attributesSvg = [ SvgAttrPx.fontSize fontSize ]
        , size = Container.relative
        , margin = config.margin
        , id = "whatever"
        }


dragBox : Config data -> data -> data -> Coordinate.System -> Svg msg
dragBox config a b system =
    let
        xAcc =
            config.xAcc

        yAcc =
            config.yAcc
    in
    Junk.rectangle system
        [ SvgAttr.fill <| Fill Colors.grayLightest ]
        (min (xAcc a) (xAcc b))
        (max (xAcc a) (xAcc b))
        (min (yAcc a) (yAcc b))
        (max (yAcc a) (yAcc b))


type Hover
    = RawX
    | RawY


hoverJunk :
    Config data
    -> data
    -> Coordinate.System
    -> Svg (Msg data)
hoverJunk config hovered sys =
    let
        xAcc =
            config.xAcc

        yAcc =
            config.yAcc

        xIsTime =
            config.xIsTime

        customLabel =
            config.labelFunc hovered

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

        mySvgText : Float -> String -> Bool -> Svg (Msg data)
        mySvgText fontHeights str bigWhite =
            let
                attributes =
                    if bigWhite then
                        [ SvgAttrPx.dy <| fontHeights * (fontSize + 2)
                        , SvgAttr.alignmentBaseline AlignmentTextBeforeEdge
                        , SvgAttr.textAnchor AnchorEnd
                        , SvgAttr.stroke Color.white
                        , SvgAttrPx.strokeWidth 5
                        ]

                    else
                        [ SvgAttrPx.dy <| fontHeights * (fontSize + 2)
                        , SvgAttr.alignmentBaseline AlignmentTextBeforeEdge
                        , SvgAttr.textAnchor AnchorEnd
                        ]
            in
            TypedSvg.text_
                attributes
                [ TypedSvg.Core.text str ]

        svgList =
            [ TypedSvg.g
                [ style "pointer-events" "none" ]
                [ mySvgText -3 customLabel True
                , mySvgText -3 customLabel False
                , mySvgText -2 textDate True
                , mySvgText -2 textDate False
                , mySvgText -1 textX True
                , mySvgText -1 textX False
                , mySvgText 0 textY True
                , mySvgText 0 textY False
                ]
            ]
    in
    Junk.placed
        sys
        (xAcc hovered)
        (yAcc hovered)
        60
        -20
        svgList


junkConfig : Config data -> State data -> Junk.Config data (Msg data)
junkConfig config state =
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
                                    config
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
                                    config
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
    Float
    -> Float
    -> Config data
    -> State data
    -> LineChart.Config data (Msg data)
chartConfig width height config state =
    { x = xAxisConfig width config state
    , y = yAxisConfig height config state
    , container = containerConfig config
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends =
        if config.showLegends then
            Legends.default

        else
            Legends.none
    , events = eventsConfig config state
    , junk = junkConfig config state
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = dotsConfig state.hovered
    }


chart : Float -> Float -> Config data -> State data -> Svg (Msg data)
chart width height config state =
    viewCustom
        (chartConfig width height config state)
        config.lines


type alias State data =
    { mouseDown : Maybe data
    , rangeX : Maybe Range
    , rangeY : Maybe Range
    , xZoom : Zoom
    , yZoom : Zoom
    , hovered : Maybe data
    , moved : Maybe data
    , movedSinceMouseDown : Int
    }


type Msg data
    = MouseDown data
    | MouseUp data
    | Hover (Maybe data)
    | Move data
    | MouseLeave
    | ResetZoom


init : State data
init =
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


newRange : Config data -> State data -> data -> XY -> ( Maybe Range, Zoom )
newRange config state mouseUp xy =
    case state.mouseDown of
        Just a ->
            let
                b =
                    mouseUp

                acc =
                    case xy of
                        X ->
                            config.xAcc

                        Y ->
                            config.yAcc

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


zoomUpdate : Config data -> State data -> data -> State data
zoomUpdate config state point =
    let
        ( rx, xc ) =
            newRange config state point X

        ( ry, yc ) =
            newRange config state point Y
    in
    { state
        | xZoom = xc
        , yZoom = yc
        , rangeX = rx
        , rangeY = ry
        , mouseDown = Nothing
        , movedSinceMouseDown = 0
    }


update : Config data -> Msg data -> State data -> State data
update config msg state =
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
                    zoomUpdate config state point

        MouseUp point ->
            case state.mouseDown of
                Just _ ->
                    zoomUpdate config state point

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

        ResetZoom ->
            { state
                | xZoom = UnZoomed
                , yZoom = UnZoomed
            }
