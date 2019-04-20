module Main exposing (main)

import Browser
import Html exposing (Html)
import Plot


type alias Point =
    { x : Float, y : Float }


points =
    [ Point 1 0
    , Point 2 1
    , Point 3 0.5
    , Point 4 -0.5
    , Point 4 2.5
    ]


type alias Model =
    { plotState : Plot.State Point }


init =
    { plotState = Plot.init }


type Msg
    = MyPlotMsg (Plot.Msg Point)


update msg model =
    case msg of
        MyPlotMsg plotMsg ->
            Plot.update plotMsg model.plotState


view : Model -> Html Msg
view model =
    Plot.drawHtml 500
        500
        (Plot.pointDefaultConfig points)
        model.plotState
        MyPlotMsg


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
