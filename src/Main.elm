module Main exposing (..)

import Browser
import Browser.Events
import Browser.Dom exposing (Viewport, Error, getViewportOf)
import Task
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


eventsConfig : Model -> Events.Config DataPoint Msg
eventsConfig model =
    Events.custom
        [ Events.onMouseDown MouseDown Events.getData
        , Events.onMouseUp MouseUp Events.getData
        , case model.mouseDown of
            Nothing ->
                Events.onMouseMove Hover Events.getNearest

            Just _ ->
                Events.onMouseMove Move Events.getData
        , Events.onMouseLeave MouseLeave
        ]


xAxisConfig : Model -> Axis.Config DataPoint msg
xAxisConfig model =
    Axis.custom
        { title = Title.default "x"
        , variable = Just << .x
        , pixels = round model.plotWidth
        , range = model.xConfig
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = ticksConfig
        }


yAxisConfig : Model -> Axis.Config DataPoint msg
yAxisConfig model =
    Axis.custom
        { title = Title.default "y"
        , variable = Just << .y
        , pixels = round model.plotHeight
        , range = model.yConfig
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


junkConfig : Model -> Junk.Config DataPoint msg
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
                            , above = hoverJunk hovered sys
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


chartConfig : Model -> Config DataPoint Msg
chartConfig model =
    { x = xAxisConfig model
    , y = yAxisConfig model
    , container = containerConfig
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.default
    , events = eventsConfig model
    , junk = junkConfig model
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = dotsConfig model.hovered
    }


chart : Model -> Lines -> Html Msg
chart model lines =
    viewCustom
        (chartConfig model)
        lines



---- MODEL ----


type alias Model =
    { mouseDown : Maybe DataPoint
    , rangeX : Maybe Range
    , rangeY : Maybe Range
    , xConfig : Range.Config
    , yConfig : Range.Config
    , hovered : Maybe DataPoint
    , moved : Maybe DataPoint
    , plotWidth : Float
    , plotHeight : Float
    }


unZoomed : Range.Config
unZoomed =
    Range.padded 20 20


init : ( Model, Cmd Msg )
init =
    ( { mouseDown = Nothing
      , rangeX = Nothing
      , rangeY = Nothing
      , xConfig = unZoomed
      , yConfig = unZoomed
      , hovered = Nothing
      , moved = Nothing
      , plotWidth = 0
      , plotHeight = 0
      }
    , attemptGetDims
    )



---- UPDATE ----


rangeDiff : List DataPoint -> (DataPoint -> Float) -> Maybe Float
rangeDiff points acc =
    let
        accessed =
            List.map acc points
    in
        Maybe.map2 (-)
            (List.maximum accessed)
            (List.minimum accessed)


type XY
    = X
    | Y


newRange : Model -> DataPoint -> XY -> ( Maybe Range, Range.Config )
newRange model mouseUp xy =
    case model.mouseDown of
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

                range =
                    case xy of
                        X ->
                            model.rangeX

                        Y ->
                            model.rangeY

                zoomMin =
                    min (acc a) (acc b)

                zoomMax =
                    max (acc a) (acc b)

                zoomDiff =
                    zoomMax - zoomMin

                ratioThreshold =
                    0.06

                zoomAreaTooSmall =
                    case range of
                        Nothing ->
                            case
                                rangeDiff data1 acc
                            of
                                Just diff ->
                                    diff * ratioThreshold > zoomDiff

                                Nothing ->
                                    True

                        Just { min, max } ->
                            (max - min) * ratioThreshold > zoomDiff
            in
                if zoomAreaTooSmall then
                    ( Nothing, unZoomed )
                else
                    ( Just <| Range zoomMin zoomMax
                    , Range.window zoomMin zoomMax
                    )

        Nothing ->
            ( Nothing, unZoomed )


type Msg
    = MouseDown DataPoint
    | MouseUp DataPoint
    | Hover (Maybe DataPoint)
    | Move DataPoint
    | MouseLeave
    | Resize Int Int
    | NewDims (Result Error Viewport)


attemptGetDims : Cmd Msg
attemptGetDims =
    Task.attempt NewDims <| getViewportOf id1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown point ->
            ( { model
                | mouseDown = Just point
                , hovered = Nothing
                , moved = Nothing
              }
            , Cmd.none
            )

        MouseUp point ->
            let
                ( rx, xc ) =
                    newRange model point X

                ( ry, yc ) =
                    newRange model point Y
            in
                ( { model
                    | xConfig = xc
                    , yConfig = yc
                    , rangeX = rx
                    , rangeY = ry
                    , mouseDown = Nothing
                  }
                , Cmd.none
                )

        Hover point ->
            ( { model | hovered = point }, Cmd.none )

        Move point ->
            ( { model | moved = Just point }, Cmd.none )

        MouseLeave ->
            ( { model | hovered = Nothing }, Cmd.none )

        Resize _ _ ->
            ( model, attemptGetDims )

        NewDims result ->
            case result of
                Ok { viewport } ->
                    ( { model
                        | plotWidth = viewport.width
                        , plotHeight = viewport.height
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | plotWidth = 1
                        , plotHeight = 1
                      }
                    , Cmd.none
                    )



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


plot : Model -> String -> Lines -> Element Msg
plot model str lines =
    el
        [ width fill
        , height fill
        , id str
        ]
    <|
        html (chart model lines)


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , padding 5
        ]
    <|
        row
            [ width fill
            , height fill
            , padding 5
            ]
            [ plot model id1 lines1
            , plot model id2 lines2
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
