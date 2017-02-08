module Main exposing (..)

import Html exposing (beginnerProgram, text, div, h1, Attribute, input, body, Html)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)
import Debug exposing (log)


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
    , mindMap : MindMap
    }


type Msg
    = Import String


init : Model
init =
    { title = "MindMap"
    , mindMap =
        { map =
            { version = "1"
            , node = []
            }
        }
    }


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
            ]
            []
        ]


wrappedView model =
    model
        |> log "render model"
        |> view


update : Msg -> Model -> Model
update msg model =
    case msg of
        Import rawString ->
            model


wrappedUpdate msg model =
    update (log "msg" msg) (log "old model" model)


main =
    beginnerProgram
        { model = init
        , view = wrappedView
        , update = wrappedUpdate
        }
