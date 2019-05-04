module ZoomPlot exposing
    ( State
    , init
    , Msg
    , update
    , points
    , draw
    , drawHtml
    , Config
    , width
    , height
    , xAxisLabel
    , yAxisLabel
    , showLegends
    , numberFormat
    , xIsTime
    , timezone
    , language
    , labelFunc
    , marginLeft
    , marginRight
    , marginTop
    , marginBottom
    , xAxisLabelOffsetX
    , xAxisLabelOffsetY
    , yAxisLabelOffsetX
    , yAxisLabelOffsetY
    , custom
    )

{-|


# Storing the plot state in your model

@docs State
@docs init


# Including `Plot.Msg` in your `Msg`

@docs Msg


# Updating your plot state

@docs update


# Drawing your line chart

@docs points
@docs draw
@docs drawHtml


# Customizing your plot

Customization is done by mutating the `Config` output from `Plot.points` and `Plot.custom` before it is inserted into `draw`.

@docs Config

Let's say that you want to customize your configuration to show legends for your different lines and also extend the right margin so that the legends don't get cut off.

Then you'll have to pipe in suitable customizers before your `draw` like this:

    Plot.points
        { toMsg = ToPlot
        , data = myPoints
        }
        |> Plot.showLegends True
        |> Plot.marginRight 70
        |> Plot.drawHtml model.plotState


# Customizers:

@docs width
@docs height
@docs xAxisLabel
@docs yAxisLabel
@docs showLegends
@docs numberFormat
@docs xIsTime
@docs timezone
@docs language
@docs labelFunc


## Margin customizers

Size in pixels of the different margins around the actual plot. This is real estate that may or may not be needed by tick labels, legends and axis labels.

@docs marginLeft
@docs marginRight
@docs marginTop
@docs marginBottom


## Axis label offset customizers

Amount in pixels for moving around the axis labels.

Positive y adjusts upwards contrary to Svg standard.

@docs xAxisLabelOffsetX
@docs xAxisLabelOffsetY
@docs yAxisLabelOffsetX
@docs yAxisLabelOffsetY


# Plotting custom types

What if your data isn't in the form of `{ x : Float, y : Float}`?

Then you will need:

@docs custom


## _Note:_

This package uses the fork [peterszerzo/line-charts](https://package.elm-lang.org/packages/peterszerzo/line-charts/latest) since it contains necessary updates to terezka's great original package.

I advise you to do the same (at least in projects using elm-zoom-plot).

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


{-| You need to store the internal state of the plot in your model.

It can be as simple as:

    import ZoomPlot as Plot

    type alias Model =
        { plotState : Plot.State Point }

or if you have multiple plots on your page:

    type alias Model =
        { plot1 : Plot.State Point
        , plot2 : Plot.State Point
        , plot3 : Plot.State Point
        }

-}
type State data
    = State
        { mouseDown : Maybe data
        , rangeX : Maybe Range
        , rangeY : Maybe Range
        , xZoom : Zoom
        , yZoom : Zoom
        , hovered : Maybe data
        , moved : Maybe data
        , movedSinceMouseDown : Int
        }


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
    State
        { mouseDown = Nothing
        , rangeX = Nothing
        , rangeY = Nothing
        , xZoom = UnZoomed
        , yZoom = UnZoomed
        , hovered = Nothing
        , moved = Nothing
        , movedSinceMouseDown = 0
        }


type alias Accessors data =
    { x : data -> Float
    , y : data -> Float
    }


{-| Include it within a constructor for easy pattern matching in your update function:

    type Msg
        = MyPlotMsg (Plot.Msg Point)
        | NoOp

or if you need routing for several plots:

    type MyPlot
        = Plot1
        | Plot2

    type Msg
        = ToPlot MyPlot (Plot.Msg Point)

-}
type Msg data
    = MouseDown (Accessors data) data
    | MouseUp (Accessors data) data
    | Hover (Maybe data)
    | Move data
    | MouseLeave
    | ResetZoom


{-| -}
type Config data msg
    = Config
        { lines : List (Series data)
        , toMsg : Msg data -> msg
        , xAcc : data -> Float
        , yAcc : data -> Float
        , pointDecoder : Point -> data
        , width : Float
        , height : Float
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


{-| Use this configuration as a starting point when your data is just a `List Point`:

    type alias Point =
        { x : Float, y : Float }

    myPoints =
        [ Point 11 120
        , Point 12 121
        , Point 13 120.5
        ]

    Plot.points
        { toMsg = ToPlot
        , data = myPoints
        }
        |> Plot.drawHtml model.plotState

-}
points :
    { toMsg :
        Msg Point -> msg
    , data : List Point
    }
    -> Config Point msg
points { toMsg, data } =
    Config
        { lines = [ LineChart.line Colors.rust Dots.circle "" data ]
        , toMsg = toMsg
        , xAcc = .x
        , yAcc = .y
        , pointDecoder = \p -> p
        , width = 800
        , height = 450
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


{-| Let's say that your data is a list of this type:

    import Time

    type alias ExampleType =
        { time : Time.Posix
        , thrust : Float
        , altitude : Float
        , latestBufferLine : String
        , errorLog : Maybe String
        }

If you want to plot data like this you can't use `Plot.points`, because it only works with data of type `List Point`.

Instead you can use `Plot.custom` like this:

    import LineChart
    import LineChart.Dots as Dots
    import LineChart.Colors as Colors

    Plot.custom
        { lines =
            [ LineChart.line
                Colors.blue
                Dots.circle
                "rocket 1"
                listOfExampleType1
            , LineChart.line
                Colors.purple
                Dots.square
                "rocket 2"
                listOfExampleType2

            ]
        , toMsg = ToPlot
        , xAcc = .time >> posixToMillis >> toFloat
        , yAcc = .thrust
        , pointDecoder = myPointDecoder
        }

The inputs `xAcc` and `yAcc` are whatever functions that will turn your type into floats to be plotted on the line chart.

Most often they are just getters plus whatever functions are needed to convert their values into floats.

You may also have noticed the `myPointDecoder` above.

The internals of the package needs a way to convert Point back to your type.

You basically have to write a function that puts the coordinates of a `Point` into an "empty" instance of your type:

    myPointDecoder : Point -> ExampleType
    myPointDecoder { x, y } =
        let
            xTime =
                x |> round |> millisToPosix
        in
        ExampleType xTime y 0 "" Nothing

-}
custom :
    { lines : List (LineChart.Series data)
    , toMsg : Msg data -> msg
    , xAcc : data -> Float
    , yAcc : data -> Float
    , pointDecoder : Point -> data
    }
    -> Config data msg
custom { lines, toMsg, xAcc, yAcc, pointDecoder } =
    Config
        { lines = lines
        , toMsg = toMsg
        , xAcc = xAcc
        , yAcc = yAcc
        , pointDecoder = pointDecoder
        , width = 800
        , height = 450
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


type alias Model data =
    { lines : List (Series data)
    , xAcc : data -> Float
    , yAcc : data -> Float
    , pointDecoder : Point -> data
    , width : Float
    , height : Float
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
    , mouseDown : Maybe data
    , rangeX : Maybe Range
    , rangeY : Maybe Range
    , xZoom : Zoom
    , yZoom : Zoom
    , hovered : Maybe data
    , moved : Maybe data
    , movedSinceMouseDown : Int
    }


toModel : Config data msg -> State data -> Model data
toModel config_ state_ =
    let
        (Config config) =
            config_

        (State state) =
            state_
    in
    { lines = config.lines
    , xAcc = config.xAcc
    , yAcc = config.yAcc
    , pointDecoder = config.pointDecoder
    , width = config.width
    , height = config.height
    , xIsTime = config.xIsTime
    , language = config.language
    , numberFormat = config.numberFormat
    , timezone = config.timezone
    , showLegends = config.showLegends
    , labelFunc = config.labelFunc
    , xAxisLabel = config.xAxisLabel
    , yAxisLabel = config.yAxisLabel
    , marginTop = config.marginTop
    , marginRight = config.marginRight
    , marginBottom = config.marginBottom
    , marginLeft = config.marginLeft
    , xAxisLabelOffsetX = config.xAxisLabelOffsetX
    , xAxisLabelOffsetY = config.xAxisLabelOffsetY
    , yAxisLabelOffsetX = config.yAxisLabelOffsetX
    , yAxisLabelOffsetY = config.yAxisLabelOffsetY
    , mouseDown = state.mouseDown
    , rangeX = state.rangeX
    , rangeY = state.rangeY
    , xZoom = state.xZoom
    , yZoom = state.yZoom
    , hovered = state.hovered
    , moved = state.moved
    , movedSinceMouseDown = state.movedSinceMouseDown
    }


{-| Use this function to place your line chart within your view.

    import Element

    view model =
        Element.layout
            [ Element.padding 20 ]
            (Plot.points
                { toMsg = ToPlot
                , data = myPoints
                }
                |> Plot.draw model.plotState
            )

-}
draw :
    State data
    -> Config data msg
    -> Element msg
draw state config =
    let
        (Config conf) =
            config
    in
    Element.map conf.toMsg
        (Element.el
            [ Element.width <| Element.px <| round conf.width
            , Element.height <| Element.px <| round conf.height
            ]
         <|
            Element.html
                (chart <| toModel config state)
        )


{-| If you, for some reason, are not using [mdgriffith/elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest) you can use this draw function instead which outputs regular `Html msg`.
-}
drawHtml :
    State data
    -> Config data msg
    -> Html msg
drawHtml state config =
    Element.layout [] <| draw state config



---- MUTATING CONFIG ----


{-| Set the plot width in pixels.
-}
width : Float -> Config data msg -> Config data msg
width n (Config config) =
    Config
        { config | width = n }


{-| Set the plot height in pixels.
-}
height : Float -> Config data msg -> Config data msg
height n (Config config) =
    Config
        { config | height = n }


{-| Set whether the x-axis should display its values as time or not.

Default is `False`.

-}
xIsTime : Bool -> Config data msg -> Config data msg
xIsTime bool (Config config) =
    Config
        { config | xIsTime = bool }


{-| Set what language dates on the time axis will be in.

Only matters when `Plot.xIsTime True`.

        import DateFormat.Language

        myPlotConfig
        |> Plot.language DateFormat.Language.swedish
        |> Plot.draw model.myPlot

Default is `DateFormat.Language.english`.

-}
language : Language -> Config data msg -> Config data msg
language lang (Config config) =
    Config
        { config | language = lang }


{-| Set the function which turns floats to strings for the axis labels (on non-time axes).

        import FormatNumber
        import FormatNumber.Locales

        myPlotConfig
        |> Plot.numberFormat
            (\float ->
              FormatNumber.format
                FormatNumber.Locales.frenchLocale
                float
            )
        |> Plot.draw model.myPlot

Default is:

        defaultFormat : Float -> String
        defaultFormat number =
            FormatNumber.format
                FormatNumber.Locales.usLocale
                number

-}
numberFormat : (Float -> String) -> Config data msg -> Config data msg
numberFormat formatter (Config config) =
    Config
        { config | numberFormat = formatter }


{-| Set which timezone your `Time.Posix` values should be converted into for your time axis tick labels.

Only matters when `Plot.xIsTime True`.

default is `Time.utc`.

-}
timezone : Time.Zone -> Config data msg -> Config data msg
timezone tz (Config config) =
    Config
        { config | timezone = tz }


{-| Set whether legends for your plot lines should be drawn or not. If you turn this on you most likely also need to adjust `marginRight` for them to not get cut off.

Default is `False`.

-}
showLegends : Bool -> Config data msg -> Config data msg
showLegends bool (Config config) =
    Config
        { config | showLegends = bool }


{-| Set extra hover label row. Hoverlabels for the x and y coordinates are always on, but if you want to add something extra on a row above them you use this.

You can for example just use a string field from your data type:

        myPlotConfig
        |> Plot.labelFunc .rocketInventorName
        |> Plot.draw model.myPlot

Default is `\_ -> ""`

-}
labelFunc : (data -> String) -> Config data msg -> Config data msg
labelFunc func (Config config) =
    Config
        { config | labelFunc = func }


{-| Set a string for labeling of the x-axis. Adjustment of margins and label offsets could be required to get the desired result.

Default is `""`

-}
xAxisLabel : String -> Config data msg -> Config data msg
xAxisLabel label (Config config) =
    Config
        { config | xAxisLabel = label }


{-| Set a string for labeling of the y-axis. Adjustment of margins and label offsets could be required to get the desired result.

Default is `""`

-}
yAxisLabel : String -> Config data msg -> Config data msg
yAxisLabel label (Config config) =
    Config
        { config | yAxisLabel = label }


{-| Default is `20`
-}
marginTop : Float -> Config data msg -> Config data msg
marginTop margin (Config config) =
    Config
        { config | marginTop = margin }


{-| Default is `30`
-}
marginRight : Float -> Config data msg -> Config data msg
marginRight margin (Config config) =
    Config
        { config | marginRight = margin }


{-| Default is `30`
-}
marginBottom : Float -> Config data msg -> Config data msg
marginBottom margin (Config config) =
    Config
        { config | marginBottom = margin }


{-| Default is `60`
-}
marginLeft : Float -> Config data msg -> Config data msg
marginLeft margin (Config config) =
    Config
        { config | marginLeft = margin }


{-| Default is `0`
-}
xAxisLabelOffsetX : Float -> Config data msg -> Config data msg
xAxisLabelOffsetX offset (Config config) =
    Config
        { config | xAxisLabelOffsetX = offset }


{-| Default is `0`
-}
xAxisLabelOffsetY : Float -> Config data msg -> Config data msg
xAxisLabelOffsetY offset (Config config) =
    Config
        { config | xAxisLabelOffsetY = -offset }


{-| Default is `0`
-}
yAxisLabelOffsetX : Float -> Config data msg -> Config data msg
yAxisLabelOffsetX offset (Config config) =
    Config
        { config | yAxisLabelOffsetX = offset }


{-| Default is `0`
-}
yAxisLabelOffsetY : Float -> Config data msg -> Config data msg
yAxisLabelOffsetY offset (Config config) =
    Config
        { config | yAxisLabelOffsetY = -offset }



---- TIME PLOTTING STUFF ----


customTimeTick : Model data -> Tick.Time -> Tick.Config msg
customTimeTick model info =
    let
        label =
            customFormatRouter model info
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


customFormatRouter : Model data -> Tick.Time -> String
customFormatRouter model info =
    case ( info.isFirst, info.change ) of
        ( True, _ ) ->
            customFormatFirst model info

        ( _, Just _ ) ->
            customFormatChange model info

        _ ->
            customFormat model info


customFormatFirst : Model data -> Tick.Time -> String
customFormatFirst model info =
    let
        lang =
            model.language

        tz =
            model.timezone
    in
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            posixToTimeWithSeconds lang tz time
                ++ format
                    [ Format.text "."
                    , Format.millisecondFixed
                    ]
                    lang
                    tz
                    time

        ( time, Tick.Second ) ->
            posixToTimeWithSeconds lang tz time

        ( time, Tick.Minute ) ->
            posixToTime lang tz time

        ( time, Tick.Hour ) ->
            posixToNameDate lang tz time
                ++ " "
                ++ posixToTime lang tz time

        ( time, Tick.Day ) ->
            posixToNameDate lang tz time

        ( time, Tick.Month ) ->
            posixToMonthNameYear lang tz time

        ( time, Tick.Year ) ->
            format
                [ Format.yearNumber ]
                lang
                tz
                time


customFormat : Model data -> Tick.Time -> String
customFormat model info =
    let
        lang =
            model.language

        tz =
            model.timezone
    in
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            format
                [ Format.text "."
                , Format.millisecondFixed
                ]
                lang
                tz
                time

        ( time, Tick.Second ) ->
            format
                [ Format.text ":"
                , Format.secondFixed
                ]
                lang
                tz
                time

        ( time, Tick.Minute ) ->
            format
                [ Format.text ":"
                , Format.minuteFixed
                ]
                lang
                tz
                time

        ( time, Tick.Hour ) ->
            posixToTime lang tz time

        ( time, Tick.Day ) ->
            posixToNameDate lang tz time

        ( time, Tick.Month ) ->
            format
                [ Format.monthNameAbbreviated ]
                lang
                tz
                time

        ( time, Tick.Year ) ->
            format
                [ Format.yearNumber ]
                lang
                tz
                time


customFormatChange : Model data -> Tick.Time -> String
customFormatChange model info =
    let
        lang =
            model.language

        tz =
            model.timezone
    in
    case ( info.timestamp, info.interval.unit ) of
        ( time, Tick.Millisecond ) ->
            posixToTimeWithSeconds lang tz time

        ( time, Tick.Second ) ->
            posixToTime lang tz time

        ( time, Tick.Minute ) ->
            posixToTime lang tz time

        ( time, Tick.Hour ) ->
            posixToNameDate lang tz time

        ( time, Tick.Day ) ->
            posixToNameDate lang tz time

        ( time, Tick.Month ) ->
            posixToMonthNameYear lang tz time

        ( time, Tick.Year ) ->
            "huh"



---- CHART ----


eventsConfig : Model data -> Events.Config data (Msg data)
eventsConfig model =
    let
        myGetData : Events.Decoder data data
        myGetData =
            Events.map model.pointDecoder Events.getData

        accs =
            { x = model.xAcc, y = model.yAcc }
    in
    Events.custom
        [ Events.onMouseDown (MouseDown accs) myGetData
        , Events.onMouseUp (MouseUp accs) myGetData
        , case model.mouseDown of
            Nothing ->
                Events.onMouseMove Hover Events.getNearest

            Just _ ->
                Events.onMouseMove Move myGetData
        , Events.onMouseLeave MouseLeave
        ]


xAxisConfig : Model data -> Axis.Config data msg
xAxisConfig model =
    Axis.custom
        { title =
            Title.atAxisMax
                model.xAxisLabelOffsetX
                model.xAxisLabelOffsetY
                model.xAxisLabel
        , variable = Just << model.xAcc
        , pixels = round model.width
        , range = setRange model.xZoom
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = xTicksConfig model
        }


yAxisConfig : Model data -> Axis.Config data msg
yAxisConfig model =
    Axis.custom
        { title =
            Title.atAxisMax
                model.yAxisLabelOffsetX
                model.yAxisLabelOffsetY
                model.yAxisLabel
        , variable = Just << model.yAcc
        , pixels = round model.height
        , range = setRange model.yZoom
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = Ticks.floatCustom 4 (customTick model.numberFormat)
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


xTicksConfig : Model data -> Ticks.Config msg
xTicksConfig model =
    if model.xIsTime then
        Ticks.custom <|
            \dataRange axisRange ->
                let
                    range =
                        case model.xZoom of
                            UnZoomed ->
                                dataRange

                            Zoomed _ _ ->
                                axisRange
                in
                List.map (customTimeTick model) (valuesWithin range)

    else
        Ticks.floatCustom 7 (customTick model.numberFormat)


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


containerConfig : Model data -> Container.Config msg
containerConfig model =
    Container.custom
        { attributesHtml = []
        , attributesSvg = [ SvgAttrPx.fontSize fontSize ]
        , size = Container.relative
        , margin =
            { top = model.marginTop
            , bottom = model.marginBottom
            , left = model.marginLeft
            , right = model.marginRight
            }
        , id = "whatever"
        }


dragBox : Model data -> data -> data -> Coordinate.System -> Svg msg
dragBox model a b system =
    let
        xAcc =
            model.xAcc

        yAcc =
            model.yAcc
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
    Model data
    -> data
    -> Coordinate.System
    -> Svg (Msg data)
hoverJunk model hovered sys =
    let
        xAcc =
            model.xAcc

        yAcc =
            model.yAcc

        customLabel =
            model.labelFunc hovered

        textDate =
            if model.xIsTime then
                TimeHelpers.posixToDate
                    model.language
                    model.timezone
                    (Time.millisToPosix <| round (xAcc hovered))

            else
                ""

        textX =
            if model.xIsTime then
                TimeHelpers.posixToTimeWithSeconds
                    model.language
                    model.timezone
                    (Time.millisToPosix <| round (xAcc hovered))

            else
                model.numberFormat (xAcc hovered)

        textY =
            model.numberFormat (yAcc hovered)

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


junkConfig : Model data -> Junk.Config data (Msg data)
junkConfig model =
    case model.mouseDown of
        Nothing ->
            case model.hovered of
                Nothing ->
                    Junk.default

                Just hovered ->
                    Junk.custom
                        (\sys ->
                            { below = []
                            , above =
                                [ hoverJunk
                                    model
                                    hovered
                                    sys
                                ]
                            , html = []
                            }
                        )

        Just downPoint ->
            case model.moved of
                Nothing ->
                    Junk.default

                Just movedPoint ->
                    Junk.custom
                        (\sys ->
                            { below =
                                [ dragBox
                                    model
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
    Model data
    -> LineChart.Config data (Msg data)
chartConfig model =
    { x = xAxisConfig model
    , y = yAxisConfig model
    , container = containerConfig model
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends =
        if model.showLegends then
            Legends.default

        else
            Legends.none
    , events = eventsConfig model
    , junk = junkConfig model
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = dotsConfig model.hovered
    }


chart : Model data -> Svg (Msg data)
chart model =
    viewCustom
        (chartConfig model)
        model.lines


type XY
    = X
    | Y


newRange : Accessors data -> State data -> data -> XY -> ( Maybe Range, Zoom )
newRange accs (State state) mouseUp xy =
    case state.mouseDown of
        Just a ->
            let
                b =
                    mouseUp

                acc =
                    case xy of
                        X ->
                            accs.x

                        Y ->
                            accs.y

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


zoomUpdate : Accessors data -> State data -> data -> State data
zoomUpdate accs ((State state_) as state) point =
    let
        ( rx, xc ) =
            newRange accs state point X

        ( ry, yc ) =
            newRange accs state point Y
    in
    State
        { state_
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
            ToPlot plotMsg ->
                { model
                    | plotState =
                        Plot.update
                            plotMsg
                            model.plotState
                }

If you need routing to update multiple plots:

    case msg of
        ToPlot Plot1 plotMsg ->
            { model
                | plot1 =
                    Plot.update
                        plotMsg
                        model.plot1
            }

        ToPlot Plot2 plotMsg ->
            { model
                | plot2 =
                    Plot.update
                        plotMsg
                        model.plot2
            }

-}
update : Msg data -> State data -> State data
update msg ((State state_) as state) =
    case msg of
        MouseDown accs point ->
            case state_.mouseDown of
                Nothing ->
                    State
                        { state_
                            | mouseDown = Just point
                            , hovered = Nothing
                            , moved = Nothing
                        }

                Just oldPoint ->
                    zoomUpdate accs state point

        MouseUp accs point ->
            case state_.mouseDown of
                Just _ ->
                    zoomUpdate accs state point

                Nothing ->
                    state

        Hover point ->
            State { state_ | hovered = point }

        Move point ->
            case state_.mouseDown of
                Nothing ->
                    State { state_ | moved = Just point }

                Just _ ->
                    let
                        oldCount =
                            state_.movedSinceMouseDown
                    in
                    State
                        { state_
                            | moved = Just point
                            , movedSinceMouseDown = oldCount + 1
                        }

        MouseLeave ->
            State { state_ | hovered = Nothing }

        ResetZoom ->
            State
                { state_
                    | xZoom = UnZoomed
                    , yZoom = UnZoomed
                }
