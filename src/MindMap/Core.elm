module MindMap.Core exposing (..)

import Animation
import Dict exposing (Dict)
import FreeMind.Decode exposing (FreeMind)


type alias Model =
    { mapText : String
    , message : Maybe String
    , mindMap : Result String MindMap
    , width : Int
    , height : Int
    , animState : List Animation.State
    , index : Int
    }


type alias MindMap =
    { mapData : FreeMind
    , nodePositionDict : Dict FreeMind.Decode.Node Position
    }


type alias Position =
    { x : Int
    , y : Int
    }


initModel : Model
initModel =
    { mapText = ""
    , message = Nothing
    , mindMap = Result.Err msgNoMapData
    , width = 800
    , height = 600
    , animState = []
    , index = 1
    }


msgNoMapData =
    "No mind map data yet."
