module Main exposing (..)

import Browser
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (frenchLocale)
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, style)
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
import LineChart.Coordinate as Coordinate
import LineChart.Axis.Title as Title
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Tick as Tick
import Svg
import Svg.Attributes exposing (fill)


---- TEST-DATA ----


xs : List Float
xs =
    List.map
        (\x -> toFloat x * 0.1)
    <|
        List.range 0 65


type alias DataPoint =
    { x : Float, y : Float }


makeData : (Float -> Float) -> List DataPoint
makeData func =
    List.map2 DataPoint xs <|
        List.map func xs


data1 : List DataPoint
data1 =
    makeData sin


data2 : List DataPoint
data2 =
    makeData cos



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
        ]


xAxisConfig : Model -> Axis.Config DataPoint msg
xAxisConfig model =
    Axis.custom
        { title = Title.default "x"
        , variable = Just << .x
        , pixels = 700
        , range = model.rangeX
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = Ticks.default
        }


yAxisConfig : Model -> Axis.Config DataPoint msg
yAxisConfig model =
    Axis.custom
        { title = Title.default "y"
        , variable = Just << .y
        , pixels = 400
        , range = model.rangeY
        , axisLine = AxisLine.rangeFrame Colors.black
        , ticks = Ticks.default
        }


containerConfig : Container.Config msg
containerConfig =
    Container.custom
        { attributesHtml = []
        , attributesSvg = [ style "font-size" "10" ]
        , size = Container.relative
        , margin = Container.Margin 40 110 50 50
        , id = "line-chart-1"
        }


dragBox : DataPoint -> DataPoint -> Coordinate.System -> Svg.Svg msg
dragBox a b system =
    Junk.rectangle system
        [ fill "rgba(200, 200, 200, 1)" ]
        a.x
        b.x
        a.y
        b.y


hoverJunk : Model -> Junk.Config DataPoint msg
hoverJunk model =
    Junk.hoverOne model.hovered
        [ ( "X", format frenchLocale << .x )
        , ( "Y", format frenchLocale << .y )
        ]


customJunk : Model -> Junk.Config DataPoint msg
customJunk model =
    case model.mouseDown of
        Nothing ->
            hoverJunk model

        Just downPoint ->
            case model.moved of
                Nothing ->
                    hoverJunk model

                Just movedPoint ->
                    Junk.custom
                        (\sys ->
                            { below =
                                [ dragBox
                                    downPoint
                                    movedPoint
                                    sys
                                ]
                            , above = []
                            , html = []
                            }
                        )


chartConfig : Model -> Config DataPoint Msg
chartConfig model =
    { x = xAxisConfig model
    , y = yAxisConfig model
    , container = containerConfig
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.default
    , events = eventsConfig model
    , junk = customJunk model
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.hoverOne model.hovered
    }


chart : Model -> Html Msg
chart model =
    viewCustom
        (chartConfig model)
        [ LineChart.line Colors.blueLight Dots.triangle "sin" data1
        , LineChart.line Colors.pinkLight Dots.circle "cos" data2
        ]



---- MODEL ----


type alias Model =
    { mouseDown : Maybe DataPoint
    , rangeX : Range.Config
    , rangeY : Range.Config
    , hovered : Maybe DataPoint
    , moved : Maybe DataPoint
    }


initRange : Range.Config
initRange =
    Range.padded 20 20


init : ( Model, Cmd Msg )
init =
    ( { mouseDown = Nothing
      , rangeX = initRange
      , rangeY = initRange
      , hovered = Nothing
      , moved = Nothing
      }
    , Cmd.none
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


newRange : Maybe DataPoint -> DataPoint -> (DataPoint -> Float) -> Range.Config
newRange ma b acc =
    case ma of
        Just a ->
            let
                zoomMin =
                    min (acc a) (acc b)

                zoomMax =
                    max (acc a) (acc b)

                rangeTooSmall =
                    case
                        rangeDiff data1 acc
                    of
                        Just diff ->
                            diff * 0.05 > zoomMax - zoomMin

                        Nothing ->
                            True
            in
                if rangeTooSmall then
                    initRange
                else
                    Range.window zoomMin zoomMax

        Nothing ->
            initRange


type Msg
    = MouseDown DataPoint
    | MouseUp DataPoint
    | Hover (Maybe DataPoint)
    | Move DataPoint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown point ->
            ( { model | mouseDown = Just point }, Cmd.none )

        MouseUp point ->
            ( { model
                | rangeX = newRange model.mouseDown point .x
                , rangeY = newRange model.mouseDown point .y
                , mouseDown = Nothing
              }
            , Cmd.none
            )

        Hover point ->
            ( { model | hovered = point }, Cmd.none )

        Move point ->
            ( { model | moved = Just point }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    chart model



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
