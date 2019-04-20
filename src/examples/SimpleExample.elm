module Main exposing (main)

import Browser
import Html exposing (Html)
import Plot


type alias Point =
    { x : Float, y : Float }


points =
    [ Point 11 120
    , Point 12 121
    , Point 13 120.5
    , Point 14 119.5
    , Point 15 121.5
    , Point 16 120.5
    , Point 17 121.5
    , Point 18 120
    , Point 19 118
    ]


type alias Model =
    { plotState : Plot.State Point }


init : Model
init =
    { plotState = Plot.init }


type Msg
    = MyPlotMsg (Plot.Msg Point)


update : Msg -> Model -> Model
update msg model =
    case msg of
        MyPlotMsg plotMsg ->
            { model
                | plotState = Plot.update myConfig plotMsg model.plotState
            }


myConfig =
    Plot.pointDefaultConfig points


view : Model -> Html Msg
view model =
    Plot.drawHtml 800
        600
        myConfig
        model.plotState
        MyPlotMsg


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
