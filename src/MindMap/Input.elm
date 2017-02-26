module MindMap.Input exposing (..)

import FreeMind.Decode
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (name, placeholder, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { mapText : String
    , message : Maybe String
    }


initModel : Model
initModel =
    { mapText = ""
    , message = Nothing
    }


type Msg
    = Import String
    | LoadSample


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Import content ->
            { model | mapText = content } ! []

        LoadSample ->
            { model | mapText = FreeMind.Decode.sampleRawString } ! []


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ name "MindMap Input"
        ]
        [ input
            [ placeholder "copy from .mm file (paste here)"
            , value model.mapText
            , onInput Import
            ]
            []
        , button [ onClick LoadSample ] [ text "Load Sample" ]
        ]
