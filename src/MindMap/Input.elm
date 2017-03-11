module MindMap.Input exposing (..)

import FreeMind.Decode
import Html exposing (Html, br, button, input, table, tbody, td, text, tr)
import Html.Attributes exposing (name, placeholder, value)
import Html.Events exposing (onClick, onInput)
import MindMap.Core exposing (Model)


type Msg
    = Import String
    | LoadSample
    | UpdateChartWidth String
    | UpdateChartHeight String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Import content ->
            { model | mapText = content } ! []

        LoadSample ->
            Import FreeMind.Decode.sampleRawString
                |> flip update model

        UpdateChartWidth text ->
            String.toInt text
                |> Result.map (\x -> { model | width = x })
                |> Result.withDefault model
                |> flip (!) []

        UpdateChartHeight text ->
            String.toInt text
                |> Result.map (\x -> { model | height = x })
                |> Result.withDefault model
                |> flip (!) []


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Html.node "mindmap-input"
        [ name "MindMap Input"
        ]
        [ input
            [ placeholder "copy from .mm file (paste here)"
            , value model.mapText
            , onInput Import
            ]
            []
        , button [ onClick LoadSample ] [ text "Load Sample" ]
        , text (Maybe.withDefault "" model.message)
        , table []
            [ tbody []
                [ tr []
                    [ td []
                        [ text "chart width"
                        ]
                    , td []
                        [ input
                            [ value (toString model.width)
                            , onInput UpdateChartWidth
                            ]
                            []
                        ]
                    ]
                , tr
                    []
                    [ td []
                        [ text "chart height"
                        ]
                    , td []
                        [ input
                            [ value (toString model.height)
                            , onInput UpdateChartHeight
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]
