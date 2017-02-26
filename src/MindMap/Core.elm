module MindMap.Core exposing (..)

import FreeMind.Decode exposing (FreeMind)


type alias Model =
    { mapText : String
    , message : Maybe String
    , map : Maybe FreeMind
    }


initModel : Model
initModel =
    { mapText = ""
    , message = Nothing
    , map = Nothing
    }
