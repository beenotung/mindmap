module MindMap.Container exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (name)
import MindMap.Chart
import MindMap.Input


type alias Model =
    { input : MindMap.Input.Model
    , chart : MindMap.Chart.Model
    }


type Msg
    = InputMsg MindMap.Input.Msg
    | ChartMsg MindMap.Chart.Msg


initModel : Model
initModel =
    { input = MindMap.Input.initModel
    , chart = MindMap.Chart.initModel
    }


view : Model -> Html Msg
view model =
    div [ name "MindMap" ]
        [ MindMap.Input.view model.input |> Html.map InputMsg
        , MindMap.Chart.view model.chart |> Html.map ChartMsg
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        InputMsg msg ->
            let
                ( newModel, cmd ) =
                    MindMap.Input.update msg model.input
            in
                ( { model | input = newModel }, cmd )

        ChartMsg msg ->
            let
                ( newModel, cmd ) =
                    MindMap.Chart.update msg model.chart
            in
                ( { model | chart = newModel }, cmd )
