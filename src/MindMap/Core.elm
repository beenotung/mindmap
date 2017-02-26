module MindMap.Core exposing (..)

import FreeMind.Decode exposing (FreeMind)


type alias Model =
    { mapText : String
    , message : Maybe String
    , map : Result String FreeMind
    , width : Int
    , height : Int
    }


initModel : Model
initModel =
    { mapText = ""
    , message = Nothing
    , map = Result.Err msgNoMapData
    , width = 800
    , height = 600
    }


msgNoMapData =
    "No mind map data yet."
