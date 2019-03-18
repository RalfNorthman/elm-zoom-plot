module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
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
import LineChart.Axis.Title as Title
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks


---- DATA ----


xs : List Float
xs =
    List.map
        (\x -> toFloat x * 0.1)
    <|
        List.range 0 65


ys : List Float
ys =
    List.map sin xs


type alias DataPoint =
    { x : Float, y : Float }


data : List DataPoint
data =
    List.map2 DataPoint xs ys


dataRangeX =
    { min = List.minimum xs
    , max = List.maximum xs
    }


dataRangeY =
    { min = List.minimum ys
    , max = List.maximum ys
    }



---- CHART ----


eventsConfig : Events.Config DataPoint Msg
eventsConfig =
    Events.custom
        [ Events.onMouseDown MouseDown Events.getData
        , Events.onMouseUp MouseUp Events.getData
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


chartConfig : Model -> Config DataPoint Msg
chartConfig model =
    { x = xAxisConfig model
    , y = yAxisConfig model
    , container = Container.responsive "line-chart-1"
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.default
    , events = eventsConfig
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.default
    }


chart : Model -> Html Msg
chart model =
    viewCustom
        (chartConfig model)
        [ LineChart.line Colors.blueLight Dots.none "sin" data ]



---- MODEL ----


type alias Model =
    { filteredData : List DataPoint
    , mouseDown : Maybe DataPoint
    , rangeX : Range.Config
    , rangeY : Range.Config
    }


initRange : Range.Config
initRange =
    Range.padded 20 20


init : ( Model, Cmd Msg )
init =
    ( { filteredData = data
      , mouseDown = Nothing
      , rangeX = initRange
      , rangeY = initRange
      }
    , Cmd.none
    )



---- UPDATE ----


newRanges : DataPoint -> DataPoint -> ( Range.Config, Range.Config )
newRanges a b =
    let
        xMax =
            max a.x b.x

        xMin =
            min a.x b.x

        yMax =
            max a.y b.y

        yMin =
            min a.y b.y

        xRangeTooSmall =
            case
                Maybe.map2 (-) dataRangeX.max dataRangeX.min
            of
                Just diff ->
                    diff * 0.05 > xMax - xMin

                Nothing ->
                    True

        yRangeTooSmall =
            case
                Maybe.map2 (-) dataRangeY.max dataRangeY.min
            of
                Just diff ->
                    diff * 0.05 > yMax - yMin

                Nothing ->
                    True

        xRange =
            if xRangeTooSmall then
                initRange
            else
                Range.window xMin xMax

        yRange =
            if yRangeTooSmall then
                initRange
            else
                Range.window yMin yMax
    in
        ( xRange, yRange )


type Msg
    = MouseDown DataPoint
    | MouseUp DataPoint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown point ->
            ( { model | mouseDown = Just point }, Cmd.none )

        MouseUp upPoint ->
            let
                downPoint =
                    Maybe.withDefault
                        (DataPoint 0 0)
                        model.mouseDown

                ( rx, ry ) =
                    newRanges downPoint upPoint
            in
                ( { model
                    | rangeX = rx
                    , rangeY = ry
                    , mouseDown = Nothing
                  }
                , Cmd.none
                )



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
