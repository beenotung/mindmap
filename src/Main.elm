module Main exposing (..)

import FreeMind.Decode
import Html exposing (Attribute, Html, body, br, button, div, h1, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)
import Dispatch


type alias Node =
    { id : String
    , text : String
    , node :
        List
            { id : String
            , text : String
            }
    }


type alias MindMap =
    { map :
        { version : String
        , node : List Node
        }
    }


type alias Model =
    { title : String
    , mindMap : Maybe MindMap
    , mapText : String
    }


type Msg
    = Import String
    | LoadSample


initModel : Model
initModel =
    { title = "MindMap"
    , mindMap = Nothing
    , mapText = ""
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Import rawString ->
            { model | mapText = rawString } ! []

        LoadSample ->
            { model | mapText = FreeMind.Decode.sampleRawString } ! []


subscriptions : model -> Sub msg
subscriptions model =
    Sub.none


cssBody =
    style [ ( "margin", "8px" ) ]



--onKeyDown tagger =
--    on "keydown" (Json.Decode.map tagger keyCode)


view : Model -> Html Msg
view model =
    body [ cssBody ]
        [ h1 [] [ text model.title ]
        , input
            [ placeholder "copy from .mm file (paste here)"
            , onInput Import
            , value model.mapText
            ]
            []
        , button
            [ onClick LoadSample
            ]
            [ text "load sample"
            ]
        , br [] []
        , text
            (case model.mindMap of
                Just mindMap ->
                    "Drawing Map"

                Nothing ->
                    "No Map"
            )
        ]


wrappedUpdate msg model =
    update (log "msg" msg) (log "old model" model)


wrappedView model =
    model
        |> log "render model"
        |> view


wrappedSubscriptions model =
    model
        |> log "subscripe"
        |> subscriptions


main =
    Html.program
        { init = initModel ! []
        , update = wrappedUpdate
        , subscriptions = wrappedSubscriptions
        , view = wrappedView
        }
