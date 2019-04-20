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
    let
        stateWithNewConfig =
            { state | config = config }
    in
    Element.map toMsg
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
         <|
            Element.html
                (chart stateWithNewConfig width height)
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


eventsConfig : State data -> Events.Config data (Msg data)
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


xAxisConfig : State data -> Float -> Axis.Config data msg
xAxisConfig state width =
    Axis.custom
        { title =
            Title.atAxisMax
                state.config.xAxisLabelOffsetX
                state.config.xAxisLabelOffsetY
                state.config.xAxisLabel
        , variable = Just << state.config.xAcc
        , pixels = round width
        , range = setRange state.xZoom
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = xTicksConfig state.config.xIsTime state.xZoom
        }


yAxisConfig : State data -> Float -> Axis.Config data msg
yAxisConfig state height =
    Axis.custom
        { title =
            Title.atAxisMax
                state.config.yAxisLabelOffsetX
                state.config.yAxisLabelOffsetY
                state.config.yAxisLabel
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
    14


containerConfig : State data -> Container.Config msg
containerConfig state =
    Container.custom
        { attributesHtml = []
        , attributesSvg = [ SvgAttrPx.fontSize fontSize ]
        , size = Container.relative
        , margin = state.config.margin
        , id = "whatever"
        }


dragBox : State data -> data -> data -> Coordinate.System -> Svg msg
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
    State data
    -> data
    -> Coordinate.System
    -> Svg (Msg data)
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


junkConfig : State data -> Junk.Config data (Msg data)
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
    State data
    -> Float
    -> Float
    -> LineChart.Config data (Msg data)
chartConfig state width height =
    { x = xAxisConfig state width
    , y = yAxisConfig state height
    , container = containerConfig state
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends =
        if state.config.showLegends then
            Legends.default

        else
            Legends.none
    , events = eventsConfig state
    , junk = junkConfig state
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = dotsConfig state.hovered
    }


chart : State data -> Float -> Float -> Svg (Msg data)
chart state width height =
    viewCustom
        (chartConfig state width height)
        state.config.lines


type alias State data =
    { mouseDown : Maybe data
    , rangeX : Maybe Range
    , rangeY : Maybe Range
    , xZoom : Zoom
    , yZoom : Zoom
    , hovered : Maybe data
    , moved : Maybe data
    , movedSinceMouseDown : Int
    , config : Config data
    }


type Msg data
    = MouseDown data
    | MouseUp data
    | Hover (Maybe data)
    | Move data
    | MouseLeave
    | UpdateConfig (Config data)
    | ResetZoom


init : Config data -> State data
init config =
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


newRange : State data -> data -> XY -> ( Maybe Range, Zoom )
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


zoomUpdate : State data -> data -> State data
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


update : Msg data -> State data -> State data
update msg state =
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

        ResetZoom ->
            { state
                | xZoom = UnZoomed
                , yZoom = UnZoomed
            }
