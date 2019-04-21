# Plot zoomable linecharts with Elm

The main objective of this package is to draw linecharts that:
* Have mouse drag zoom
* Have reasonable time axes

## Minimal example:

```elm
import ZoomPlot as Plot


type alias Point =
    { x : Float, y : Float }


points =
    [ Point 11 120
    , Point 12 121
    , Point 13 120.5
    ]


myConfig =
    Plot.easyConfig points


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


view : Model -> Html Msg
view model =
    Plot.drawHtml
        800
        600
        myConfig
        model.plotState
        MyPlotMsg
```
