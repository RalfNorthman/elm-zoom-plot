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
import FormatNumber
import FormatNumber.Locales
import Html exposing (Html)
import LineChart
import LineChart.Colors as Colors
import LineChart.Coordinate
import LineChart.Dots as Dots
import Task exposing (Task)
import Time exposing (Posix)
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



---- PROGRAM ----


myPointDecoder { x, y } =
    Record x y Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0


view : Model -> Html Msg
view model =
    layout [ padding 20 ] <|
        el
            [ width <| px 800
            , height <| px 600
            ]
            (Plot.custom
                { lines = [ LineChart.line Colors.rust Dots.circle "" Data.records ]
                , toMsg = ToPlot
                , xAcc = .time >> .posixToMillis >> toFloat
                , yAcc = .ben
                , pointDecoder = myPoints
                }
                |> Plot.width 1920
                |> Plot.height 400
                |> Plot.draw model.plotState
            )


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
