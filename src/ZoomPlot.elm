module ZoomPlot exposing
    ( easyConfig
    , State
    , init
    , Msg
    , draw
    , drawHtml
    , update
    , defaultConfigWith
    , Config
    )

{-|


# Minimal configuration

@docs easyConfig


# Storing the plot state in your model

@docs State
@docs init


# Including `Plot.Msg` in your `Msg`

@docs Msg


# Plotting your linechart

@docs draw
@docs drawHtml


# Updating your plot state

@docs update


# Configuration

@docs defaultConfigWith

Now, if you want to change anything from your default configuration you need to know about the other fields of the `Plot.Config` type:

@docs Config

-}

import Color
import DateFormat as Format
import DateFormat.Language exposing (Language, english)
import Element exposing (Element)
import FormatNumber
import FormatNumber.Locales exposing (Locale, usLocale)
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
import Svg exposing (Svg)
import Time exposing (Posix)
import TimeHelpers exposing (..)
import TypedSvg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as SvgAttrPx
import TypedSvg.Core
import TypedSvg.Types exposing (..)


defaultFormat : Float -> String
defaultFormat number =
    FormatNumber.format
        usLocale
        number


{-| So, what does all this mean?

  - **lines** :
    A list where you set how your different "lines" on your chart should look: color, point shape and legend name.

        import LineChart
        import LineChart.Colors as Colors
        import LineChart.Dots as Dots

        [ LineChart.line Colors.tealLight Dots.circle "cos" myCosData
        , LineChart.line Colors.goldLight Dots.diamond "polysin" myPolySinData
        ]

    _Note:_ This package use the fork [peterszerzo/line-charts](https://package.elm-lang.org/packages/peterszerzo/line-charts/latest) since it contains necessary updates from Tereza Sokols (terezka) original package. I advise you do the same if you don't want unnecessary duplication among your dependencies.

-}
type alias Config data =
    { lines : List (Series data)
    , xAcc : data -> Float
    , yAcc : data -> Float
    , pointDecoder : Point -> data
    , xIsTime : Bool
    , language : Language
    , numberFormat : Float -> String
    , timezone : Time.Zone
    , showLegends : Bool
    , labelFunc : data -> String
    , xAxisLabel : String
    , yAxisLabel : String
    , marginTop : Float
    , marginRight : Float
    , marginBottom : Float
    , marginLeft : Float
    , xAxisLabelOffsetX : Float
    , xAxisLabelOffsetY : Float
    , yAxisLabelOffsetX : Float
    , yAxisLabelOffsetY : Float
    }


{-| If you want to plot data that is not of type `Point` you can't use `easyConfig`. Instead you use `defaultConfigWith` to create your starting point configuration.

    type alias ExampleType =
        { time : Posix
        , value : Float
        , text : String
        , otherValue : Float
        }

You need to supply it with two accessor functions (getters), one for each chart coordinate (x and y).

    myDefaultConfig : Plot.Config ExampleType
    myDefaultConfig =
        Plot.defaultConfigWith
            listOfExampleTypes
            (.time >> Time.posixToMillis >> toFloat)
            .value
            myPointDecoder

It also needs a _point decoder_. The internals of the package needs a way to convert Point back to your type. You basically have to write a function that puts the coordinates of a `Point` into an "empty" instance of your type.

    myPointDecoder { x, y } =
        let
            decodedX =
                x |> floor |> Time.millisToPosix
        in
        ExampleType decodedX y "" 0

-}
defaultConfigWith :
    List data
    -> (data -> Float)
    -> (data -> Float)
    -> (Point -> data)
    -> Config data
defaultConfigWith dataList xAcc yAcc pointDecoder =
    { lines = [ LineChart.line Colors.tealLight Dots.circle "" dataList ]
    , xAcc = xAcc
    , yAcc = yAcc
    , pointDecoder = pointDecoder
    , xIsTime = False
    , language = english
    , numberFormat = defaultFormat
    , timezone = Time.utc
    , showLegends = False
    , labelFunc = \_ -> ""
    , xAxisLabel = ""
    , yAxisLabel = ""
    , marginTop = 20
    , marginRight = 30
    , marginBottom = 30
    , marginLeft = 60
    , xAxisLabelOffsetX = 0
    , xAxisLabelOffsetY = 0
    , yAxisLabelOffsetX = 0
    , yAxisLabelOffsetY = 0
    }


{-| Use this configuration as a starting point when your data is just a `List Point`:

    type alias Point =
        { x : Float, y : Float }

    points =
        [ Point 11 120
        , Point 12 121
        , Point 13 120.5
        ]

    myConfig =
        Plot.easyConfig points

-}
easyConfig : List Point -> Config Point
easyConfig points =
    { lines = [ LineChart.line Colors.tealLight Dots.circle "" points ]
    , xAcc = .x
    , yAcc = .y
    , pointDecoder = \p -> p
    , xIsTime = False
    , language = english
    , numberFormat = defaultFormat
    , timezone = Time.utc
    , showLegends = False
    , labelFunc = \_ -> ""
    , xAxisLabel = ""
    , yAxisLabel = ""
    , marginTop = 20
    , marginRight = 30
    , marginBottom = 30
    , marginLeft = 60
    , xAxisLabelOffsetX = 0
    , xAxisLabelOffsetY = 0
    , yAxisLabelOffsetX = 0
    , yAxisLabelOffsetY = 0
    }


{-| Use this function to place your linechart within your view.

    import Element as E

    view model =
        E.layout [ E.padding 20 ] <|
            E.el
                [ E.width <| px 800
                , E.height <| px 600
                ]
            <|
                Plot.draw
                    800
                    600
                    myConfig
                    model.plotState
                    MyPlotMsg

The first two arguments are width and height, which mostly determines the aspect ratio of your plot since it fills the container you place it in.

Small dimension values in large containers can make the text in the plot unreasonably large.

-}
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


{-| If you for some reason are not using [mdgriffith/elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest) you can use this draw function instead which outputs regular `Html msg`.
-}
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


customTimeTick : Config data -> Tick.Time -> Tick.Config msg
customTimeTick config info =
    let
        label =
            customFormatRouter config info
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


customFormatRouter : Config data -> Tick.Time -> String
customFormatRouter config info =
    case ( info.isFirst, info.change ) of
        ( True, _ ) ->
            customFormatFirst config info

        ( _, Just _ ) ->
            customFormatChange config info

        _ ->
            customFormat config info


customFormatFirst : Config data -> Tick.Time -> String
customFormatFirst { language, timezone } info =
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            posixToTimeWithSeconds language timezone time
                ++ format
                    [ Format.text "."
                    , Format.millisecondFixed
                    ]
                    language
                    timezone
                    time

        ( time, Tick.Second ) ->
            posixToTimeWithSeconds language timezone time

        ( time, Tick.Minute ) ->
            posixToTime language timezone time

        ( time, Tick.Hour ) ->
            posixToNameDate language timezone time
                ++ " "
                ++ posixToTime language timezone time

        ( time, Tick.Day ) ->
            posixToNameDate language timezone time

        ( time, Tick.Month ) ->
            posixToMonthNameYear language timezone time

        ( time, Tick.Year ) ->
            format
                [ Format.yearNumber ]
                language
                timezone
                time


customFormat : Config data -> Tick.Time -> String
customFormat { language, timezone } info =
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            format
                [ Format.text "."
                , Format.millisecondFixed
                ]
                language
                timezone
                time

        ( time, Tick.Second ) ->
            format
                [ Format.text ":"
                , Format.secondFixed
                ]
                language
                timezone
                time

        ( time, Tick.Minute ) ->
            format
                [ Format.text ":"
                , Format.minuteFixed
                ]
                language
                timezone
                time

        ( time, Tick.Hour ) ->
            posixToTime language timezone time

        ( time, Tick.Day ) ->
            posixToNameDate language timezone time

        ( time, Tick.Month ) ->
            format
                [ Format.monthNameAbbreviated ]
                language
                timezone
                time

        ( time, Tick.Year ) ->
            format
                [ Format.yearNumber ]
                language
                timezone
                time


customFormatChange : Config data -> Tick.Time -> String
customFormatChange { language, timezone } info =
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            posixToTimeWithSeconds language timezone time

        ( time, Tick.Second ) ->
            posixToTime language timezone time

        ( time, Tick.Minute ) ->
            posixToTime language timezone time

        ( time, Tick.Hour ) ->
            posixToNameDate language timezone time

        ( time, Tick.Day ) ->
            posixToNameDate language timezone time

        ( time, Tick.Month ) ->
            posixToMonthNameYear language timezone time

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
        , ticks = xTicksConfig config state.xZoom
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
        , ticks = Ticks.floatCustom 4 (customTick config.numberFormat)
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


xTicksConfig : Config data -> Zoom -> Ticks.Config msg
xTicksConfig config zoom =
    if config.xIsTime then
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
                List.map (customTimeTick config) (valuesWithin range)

    else
        Ticks.floatCustom 7 (customTick config.numberFormat)


customTick : (Float -> String) -> Float -> Tick.Config msg
customTick nrFormat value =
    let
        label =
            Junk.label Colors.black (nrFormat value)
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
        , margin =
            { top = config.marginTop
            , bottom = config.marginBottom
            , left = config.marginLeft
            , right = config.marginRight
            }
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
                    config.language
                    config.timezone
                    (Time.millisToPosix <| round (xAcc hovered))

            else
                ""

        textX =
            if xIsTime then
                TimeHelpers.posixToTimeWithSeconds
                    config.language
                    config.timezone
                    (Time.millisToPosix <| round (xAcc hovered))

            else
                config.numberFormat (xAcc hovered)

        textY =
            config.numberFormat (yAcc hovered)

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


{-| You need to store the internal state of the plot in your model.

It can be as simple as:

    type alias Model =
        { plotState : Plot.State Point }

or if you have multiple plots on your page:

    type alias Model =
        { plot1 : Plot.State Point
        , plot2 : Plot.State Point
        , plot3 : Plot.State Point
        }

-}
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


{-| Include it within a constructor for easy pattern matching in your update function:

    type Msg
        = MyPlotMsg (Plot.Msg Point)
        | MsgNotConcerningPlots

or if you need routing for several plots:

    type PlotNr
        = Plot1
        | Plot2
        | Plot3

    type Msg
        = ToPlot PlotNr (Plot.Msg ExampleType)

-}
type Msg data
    = MouseDown data
    | MouseUp data
    | Hover (Maybe data)
    | Move data
    | MouseLeave
    | ResetZoom


{-| All plots have the same initial state regardless of individual configuration.

    init : Model
    init =
        { plot1 = Plot.init
        , plot2 = Plot.init
        , plot3 = Plot.init
        }

-}
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


{-| Naturally you need to handle the plot messages in your update function. This is what `Plot.update` is for.

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            MyPlotMsg plotMsg ->
                { model
                    | plotState =
                        Plot.update
                            myConfig
                            plotMsg
                            model.plotState
                }

If you need routing to update multiple plots:

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ToPlot Plot1 plotMsg ->
                ( { model
                    | plot1 =
                        Plot.update
                            plotConfig1
                            plotMsg
                            model.plot1
                  }
                , Cmd.none
                )

            ToPlot Plot2 plotMsg ->
                ( { model
                    | plot2 =
                        Plot.update
                            plotConfig2
                            plotMsg
                            model.plot2
                  }
                , Cmd.none
                )

-}
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
