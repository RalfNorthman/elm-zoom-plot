module Main exposing (main)

import Browser
import Browser.Dom exposing (Error, Viewport, getViewport)
import Browser.Events
import Color exposing (toRgba)
import Data
import DateFormat.Language
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FormatNumber
import FormatNumber.Locales
import Html exposing (Html)
import LineChart
import LineChart.Colors as Colors
import LineChart.Coordinate
import LineChart.Dots as Dots
import Maybe.Extra
import Task exposing (Task)
import Time exposing (Month(..), Posix, millisToPosix, posixToMillis)
import Time.Extra
import ZoomPlot as Plot


type alias Record =
    { date : Posix
    , ben : Maybe Float
    , ch4 : Maybe Float
    , co : Maybe Float
    , ebe : Maybe Float
    , nmhc : Maybe Float
    , no : Maybe Float
    , no_2 : Maybe Float
    , nox : Maybe Float
    , o_3 : Maybe Float
    , pm10 : Maybe Float
    , pm25 : Maybe Float
    , so_2 : Maybe Float
    , tch : Maybe Float
    , tol : Maybe Float
    , station : Int
    }


type alias Point =
    { x : Float, y : Float }


myFilter : (Record -> Maybe Float) -> Int -> List Record
myFilter acc day =
    let
        from : Posix
        from =
            Time.Extra.Parts 2018 Mar 1 13 0 0 0
                |> Time.Extra.partsToPosix Time.utc

        to : Posix
        to =
            Time.Extra.Parts 2018 Mar day 2 0 0 0
                |> Time.Extra.partsToPosix Time.utc

        between : Posix -> Posix -> Posix -> Bool
        between a b x =
            (posixToMillis x > posixToMillis a)
                && (posixToMillis x < posixToMillis b)
    in
    Data.records
        |> List.filter
            (\r -> acc r |> Maybe.Extra.isJust)
        |> List.filter
            (\r -> between from to r.date)


recordsNO : List Record
recordsNO =
    myFilter .no 15


recordsSO2 : List Record
recordsSO2 =
    myFilter .so_2 30



---- MODEL ----


type alias Model =
    { plotNO : Plot.State Record
    , plotSO2 : Plot.State Record
    }


init : ( Model, Cmd msg )
init =
    ( { plotNO = Plot.init
      , plotSO2 = Plot.init
      }
    , Cmd.none
    )



---- UPDATE ----


type MyPlot
    = PlotNO
    | PlotSO2


type Msg
    = ToPlot MyPlot (Plot.Msg Record)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ToPlot PlotNO plotMsg ->
            ( { model | plotNO = Plot.update plotMsg model.plotNO }, Cmd.none )

        ToPlot PlotSO2 plotMsg ->
            ( { model | plotSO2 = Plot.update plotMsg model.plotSO2 }, Cmd.none )



---- VIEW ----


googleFont : String -> Attribute Msg
googleFont fontName =
    let
        fontString =
            String.replace " " "+" fontName
    in
    Font.family
        [ Font.external
            { url =
                "https://fonts.googleapis.com/css?family="
                    ++ fontString
            , name = fontName
            }
        ]


displaySerif : Attribute Msg
displaySerif =
    googleFont "Playfair Display"


thinSans : Attribute Msg
thinSans =
    googleFont "Gruppo"


plotSans : Attribute Msg
plotSans =
    googleFont "Montserrat"


separator : Attribute Msg
separator =
    googleFont "Nixie One"


pointDecoderNO : Point -> Record
pointDecoderNO { x, y } =
    Record (x |> round |> millisToPosix) Nothing Nothing Nothing Nothing Nothing (Just y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0


pointDecoderSO2 : Point -> Record
pointDecoderSO2 { x, y } =
    Record (x |> round |> millisToPosix) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just y) Nothing Nothing 0


myPlotWidth =
    800


myPlotHeight =
    280


titleKaggleLink : Element Msg
titleKaggleLink =
    el [ height fill, width fill ] <|
        newTabLink [ centerX, centerY ]
            { url =
                "https://www.kaggle.com/decide-soluciones/air-quality-madrid"
            , label =
                column [ spacing 2 ]
                    [ el
                        [ centerX
                        , displaySerif
                        , Font.size 24
                        ]
                      <|
                        text "Madrid Air Quality"
                    , el
                        [ centerX
                        , thinSans
                        , Font.size 27
                        , Font.wordSpacing -2
                        ]
                      <|
                        text "Plaza de España"
                    ]
            }


plotNO : Model -> Element Msg
plotNO model =
    Plot.custom
        { lines =
            [ LineChart.line
                Colors.purple
                Dots.circle
                ""
                recordsNO
            ]
        , toMsg = ToPlot PlotNO
        , xAcc = .date >> posixToMillis >> toFloat
        , yAcc = .no >> Maybe.withDefault 0
        , pointDecoder = pointDecoderNO
        }
        |> Plot.width myPlotWidth
        |> Plot.height myPlotHeight
        |> Plot.xIsTime True
        |> Plot.marginLeft 70
        |> Plot.marginRight 40
        |> Plot.yAxisLabel "NO [μg/m³]"
        |> Plot.yAxisLabelOffsetX 30
        |> Plot.yAxisLabelOffsetY 20
        |> Plot.draw model.plotNO


plotSO2 : Model -> Element Msg
plotSO2 model =
    Plot.custom
        { lines =
            [ LineChart.line
                Colors.blue
                Dots.circle
                ""
                recordsSO2
            ]
        , toMsg = ToPlot PlotSO2
        , xAcc = .date >> posixToMillis >> toFloat
        , yAcc = .so_2 >> Maybe.withDefault 0
        , pointDecoder = pointDecoderSO2
        }
        |> Plot.width myPlotWidth
        |> Plot.height myPlotHeight
        |> Plot.xIsTime True
        |> Plot.marginLeft 70
        |> Plot.marginRight 40
        |> Plot.marginTop 50
        |> Plot.yAxisLabel "SO₂ [μg/m³]"
        |> Plot.yAxisLabelOffsetX 35
        |> Plot.yAxisLabelOffsetY 20
        |> Plot.draw model.plotSO2


signatureDocumentationLink : Element Msg
signatureDocumentationLink =
    el
        [ centerX
        , height fill
        ]
    <|
        link [ centerY ]
            { url =
                "https://package.elm-lang.org/packages/RalfNorthman/elm-zoom-plot/latest"
            , label =
                paragraph [ Font.center, padding 20 ]
                    [ el [ displaySerif ] <|
                        text "Ralf Northman "
                    , el
                        [ separator
                        , Font.size 35
                        ]
                      <|
                        text "|"
                    , el
                        [ thinSans
                        , Font.size 25
                        ]
                      <|
                        text " elm-zoom-plot"
                    ]
            }


view : Model -> Html Msg
view model =
    layout
        [ padding 20
        ]
    <|
        column
            [ spacing 2
            , width fill
            , height fill
            ]
            [ titleKaggleLink
            , el [ plotSans, centerX ] <| plotNO model
            , el [ plotSans, centerX ] <| plotSO2 model
            , signatureDocumentationLink
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
