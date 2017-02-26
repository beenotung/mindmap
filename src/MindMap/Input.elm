module MindMap.Input exposing (..)

import FreeMind.Decode
import Html exposing (Html, button, input, text)
import Html.Attributes exposing (name, placeholder, value)
import Html.Events exposing (onClick, onInput)
import MindMap.Core exposing (Model)


type Msg
    = Import String
    | LoadSample


update : Msg -> Model -> Model
update msg model =
    case msg of
        Import content ->
            { model | mapText = content }

        LoadSample ->
            Import FreeMind.Decode.sampleRawString
                |> flip update model


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
        ]
