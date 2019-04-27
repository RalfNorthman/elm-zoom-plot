module ZoomPlot exposing
    ( State
    , init
    , Msg
    , draw
    , drawHtml
    , update
    , Config
    , custom, height, labelFunc, language, marginBottom, marginLeft, marginRight, marginTop, numberFormat, points, showLegends, timezone, width, xAxisLabel, xAxisLabelOffsetX, xAxisLabelOffsetY, xIsTime, yAxisLabel, yAxisLabelOffsetX, yAxisLabelOffsetY
    )

{-|


# Minimal configuration

@docs easyConfig


# Storing the plot state in your model

@docs State
@docs init


# Including `Plot.Msg` in your `Msg`

@docs Msg


# Drawing your linechart

@docs draw
@docs drawHtml


# Updating your plot state

@docs update


# Customizing your plot

Let's say that you want to customize your configuration to show legends for your different lines and also extend the right margin so that the legends don't get cut off. You can do it like this:

    myConfig =
        let
            default =
                Plot.easyConfig points
        in
        { default
            | showLegends = True
            , marginRight = 70
        }

What if your data isn't in the form of `{ x : Float, y : Float}`?

Then you will need:

@docs defaultConfigWith

If you want to start customizing every aspect of your plot you need to know about all the fields of the `Plot.Config` type:

@docs Config

So, what does all this mean?

  - **lines** :
    A list where you set how the different "lines" on your chart should look: color, point shape and legend name. Most importantly it is also here you supply your data to the configuration.

        import LineChart
        import LineChart.Colors as Colors
        import LineChart.Dots as Dots

        [ LineChart.line
            Colors.tealLight
            Dots.circle
            "cos"
            myCosData
        , LineChart.line
            Colors.goldLight
            Dots.diamond
            "polysin"
            myPolySinData
        ]

    _Note:_ This package uses the fork [peterszerzo/line-charts](https://package.elm-lang.org/packages/peterszerzo/line-charts/latest) since it contains necessary updates to terezka's great original package. I advise you to do the same, otherwise namespace collisions seem inevitable.

  - **xAcc** :
    Accessor function (getter) from your data type to the float value used for x coordinates. If you want a time axis (don't forget to set `xIsTime` to `True`) for your `Time.Posix` values you can do something like this:

        .time >> Time.posixToMillis >> toFloat

  - **yAcc** :
    Accessor function (getter) from your data type to the float value used for y coordinates.

        .rocketThrust

  - **pointDecoder** :
    The internals of the package needs a way to convert Point back to your type. You basically have to write a function that puts the coordinates of a `Point` into an "empty" instance of your type.

        \{ x, y } -> RocketData 0 0 "" x 0 "" 0 y Nothing

  - **xIsTime** :
    Whether the x-axis should display its values as time or not.

        True

  - **language** :
    What language dates on time axes will be in.

        import DateFormat.Language

        DateFormat.Language.swedish

  - **numberFormat** :
    The function which turns floats to strings for the axis labels (on non-time axes).

        import FormatNumber
        import FormatNumber.Locales

        \float ->
            FormatNumber.format
                FormatNumber.Locales.frenchLocale
                float

  - **timeZone** :
    Which timezone your `Time.Posix` values should be converted into for your time axis.

        import Time

        Time.utc -- This is the default.

  - **showLegends** :
    Whether legends for your plot lines should be drawn. If you turn this on you most likely also need to adjust `marginRight` for them to not get cut off.

        True

  - **labelFunc** :
    Hoverlabels for the x and y coordinates are always on, but if you want to add something extra on a row above them you use this field. One of the more straightforward ways to use this is to just use a string field from your data type:

        .rocketInventorName

    or something more involved:

        \d -> d.firstName ++ " " ++ d.lastName

  - **xAxisLabel** / **yAxisLabel** :
    Strings for labeling the axes. Adjustment of margins and label offsets could be required to get the desired result.

        "Rocket velocity [km/s]"

  - **marginTop** / **marginRight** / **marginBottom** / **marginLeft** :
    Size in pixels of the different margins around the actual plot. This is real estate that may or may not be needed by tick labels, legends and axis labels.

        60

  - **xAxisLabelOffsetX** / **xAxisLabelOffsetY** / **yAxisLabelOffsetX** / **yAxisLabelOffsetY** :
    Distance in pixels of how much you want to adjust the positioning of the axis title labels.

        20


# Configuration defaults

It can be useful to know what the defaults are:

        { lines = [ LineChart.line
                        Colors.tealLight
                        Dots.circle
                        ""
                        inputPoints
                  ]
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

        defaultFormat : Float -> String
        defaultFormat number =
            FormatNumber.format
                FormatNumber.Locales.usLocale
                number

Above are the defaults from `easyConfig`, where `inputPoints` comes from the user. The defaults for `defaultConfigWith` are the same except the fields `xAcc`, `yAcc` and `pointDecoder` which are user-provided as well.

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

    points =
        [ Point 11 120
        , Point 12 121
        , Point 13 120.5
        ]

    myConfig =
        Plot.easyConfig points

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

    type alias ExampleType =
        { time : Posix
        , value : Float
        , text : String
        , otherValue : Float
        }

If you want to plot data like this you can't use `easyConfig`, because it only works with data of type `Point`.
Instead you will have to use `defaultConfigWith` to create your starting point configuration.

You need to supply it with two accessor functions (getters), one for each chart coordinate (x and y).

    myDefaultConfig : Plot.Config ExampleType
    myDefaultConfig =
        Plot.defaultConfigWith
            listOfExampleTypes
            (.time >> Time.posixToMillis >> toFloat)
            .value
            myPointDecoder

It also needs a _point decoder_. The internals of the package needs a way to convert Point back to your type. You basically have to write a function that puts the coordinates of a `Point` into an "empty" instance of your type:

    myPointDecoder { x, y } =
        let
            decodedX =
                x |> floor |> Time.millisToPosix
        in
        ExampleType decodedX y "" 0

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
    State data
    -> Config data msg
    -> Element msg
draw state ((Config { toMsg }) as config) =
    Element.map toMsg
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
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


width : Float -> Config data msg -> Config data msg
width n (Config config) =
    Config
        { config | width = n }


height : Float -> Config data msg -> Config data msg
height n (Config config) =
    Config
        { config | height = n }


xIsTime : Bool -> Config data msg -> Config data msg
xIsTime bool (Config config) =
    Config
        { config | xIsTime = bool }


language : Language -> Config data msg -> Config data msg
language lang (Config config) =
    Config
        { config | language = lang }


numberFormat : (Float -> String) -> Config data msg -> Config data msg
numberFormat formatter (Config config) =
    Config
        { config | numberFormat = formatter }


timezone : Time.Zone -> Config data msg -> Config data msg
timezone tz (Config config) =
    Config
        { config | timezone = tz }


showLegends : Bool -> Config data msg -> Config data msg
showLegends bool (Config config) =
    Config
        { config | showLegends = bool }


labelFunc : (data -> String) -> Config data msg -> Config data msg
labelFunc func (Config config) =
    Config
        { config | labelFunc = func }


xAxisLabel : String -> Config data msg -> Config data msg
xAxisLabel label (Config config) =
    Config
        { config | xAxisLabel = label }


yAxisLabel : String -> Config data msg -> Config data msg
yAxisLabel label (Config config) =
    Config
        { config | yAxisLabel = label }


marginTop : Float -> Config data msg -> Config data msg
marginTop margin (Config config) =
    Config
        { config | marginTop = margin }


marginRight : Float -> Config data msg -> Config data msg
marginRight margin (Config config) =
    Config
        { config | marginRight = margin }


marginBottom : Float -> Config data msg -> Config data msg
marginBottom margin (Config config) =
    Config
        { config | marginBottom = margin }


marginLeft : Float -> Config data msg -> Config data msg
marginLeft margin (Config config) =
    Config
        { config | marginLeft = margin }


xAxisLabelOffsetX : Float -> Config data msg -> Config data msg
xAxisLabelOffsetX offset (Config config) =
    Config
        { config | xAxisLabelOffsetX = offset }


xAxisLabelOffsetY : Float -> Config data msg -> Config data msg
xAxisLabelOffsetY offset (Config config) =
    Config
        { config | xAxisLabelOffsetY = offset }


yAxisLabelOffsetX : Float -> Config data msg -> Config data msg
yAxisLabelOffsetX offset (Config config) =
    Config
        { config | yAxisLabelOffsetX = offset }


yAxisLabelOffsetY : Float -> Config data msg -> Config data msg
yAxisLabelOffsetY offset (Config config) =
    Config
        { config | yAxisLabelOffsetY = offset }



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
