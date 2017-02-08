module Main exposing (..)

import Html exposing (beginnerProgram, text, div, h1, Attribute)


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


init : Model
init =
    { title = "title"
    , mindMap =
        { map =
            { version = "1"
            , node = []
            }
        }
    }


view model =
    div []
        [ h1 [] [ text model.title ] ]


update msg model =
    model


main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }
