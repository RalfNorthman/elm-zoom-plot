module Main exposing (main)

import Browser
import Element as E
import Html exposing (Html)
import ZoomPlot as Plot


type alias Point =
    { x : Float, y : Float }


myPoints =
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
    = ToPlot (Plot.Msg Point)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToPlot plotMsg ->
            { model
                | plotState = Plot.update plotMsg model.plotState
            }


view : Model -> Html Msg
view model =
    E.layout [ E.padding 20 ] <|
        E.el
            [ E.width <| E.px 800
            , E.height <| E.px 600
            ]
            (Plot.points
                { toMsg = ToPlot
                , data = myPoints
                }
                |> Plot.width 1920
                |> Plot.height 400
                |> Plot.draw model.plotState
            )


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
