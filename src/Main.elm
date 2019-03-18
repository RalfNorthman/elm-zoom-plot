module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import LineChart exposing (..)
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line


---- DATA ----


xs =
    List.map
        (\x -> toFloat x * 0.1)
    <|
        List.range 0 65


ys =
    List.map sin xs


type alias DataPoint =
    { x : Float, y : Float }


data =
    List.map2 DataPoint xs ys



---- CHART ----


decoder =
    Events.map2 Tuple.pair Events.getNearest Events.getData


eventsConfig : Events.Config DataPoint Msg
eventsConfig =
    Events.custom
        [ Events.onMouseDown MouseDown Events.getData
        , Events.onMouseUp MouseUp Events.getData
        ]


chartConfig : Config DataPoint Msg
chartConfig =
    { x = Axis.default 700 "x" .x
    , y = Axis.default 400 "y" .y
    , container = Container.default "line-chart-1"
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


chart : List DataPoint -> Html Msg
chart points =
    viewCustom chartConfig
        [ LineChart.line Colors.blueLight Dots.none "sin" points ]



---- MODEL ----


type alias Model =
    { filteredData : List DataPoint
    , mouseDown : Maybe DataPoint
    }


init : ( Model, Cmd Msg )
init =
    ( { filteredData = data
      , mouseDown = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


filterDownUp : DataPoint -> DataPoint -> List DataPoint -> List DataPoint
filterDownUp a b list =
    let
        x_max =
            max a.x b.x

        x_min =
            min a.x b.x

        y_max =
            max a.y b.y

        y_min =
            min a.y b.y

        myTest : DataPoint -> Bool
        myTest p =
            p.x
                > x_min
                && p.x
                < x_max
                && p.y
                > y_min
                && p.y
                < y_max
    in
        list |> List.filter myTest


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

                filtered =
                    case
                        filterDownUp downPoint upPoint data
                    of
                        [] ->
                            data

                        list ->
                            list
            in
                ( { model
                    | filteredData = filtered
                    , mouseDown = Nothing
                  }
                , Cmd.none
                )



---- VIEW ----


view : Model -> Html Msg
view model =
    chart model.filteredData



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
