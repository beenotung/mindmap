module MindMap.Container exposing (..)

import FreeMind.Decode exposing (node)
import Html exposing (Html, div)
import Html.Attributes exposing (id, name)
import MindMap.Chart
import MindMap.Core exposing (Model)
import MindMap.Input


type Msg
    = InputMsg MindMap.Input.Msg
    | ChartMsg MindMap.Chart.Msg


view : Model -> Html Msg
view model =
    Html.node "mindmap"
        []
        [ MindMap.Input.view model |> Html.map InputMsg
        , Html.br [] []
        , MindMap.Chart.view model |> Html.map ChartMsg
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        InputMsg msg ->
            let
                ( model1, cmd1 ) =
                    MindMap.Input.update msg model

                ( model2, cmd2 ) =
                    update (ChartMsg MindMap.Chart.Init) model1
            in
                ( model2, cmd2 )

        ChartMsg msg ->
            MindMap.Chart.update msg model
