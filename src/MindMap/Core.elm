module MindMap.Core exposing (..)

import Animation
import FreeMind.Decode exposing (FreeMind)


type alias Model =
    { mapText : String
    , message : Maybe String
    , map : Result String FreeMind
    , width : Int
    , height : Int
    , animState : List Animation.State
    , index : Int
    }


initModel : Model
initModel =
    { mapText = ""
    , message = Nothing
    , map = Result.Err msgNoMapData
    , width = 800
    , height = 600
    , animState = []
    , index = 1
    }


msgNoMapData =
    "No mind map data yet."
