module FreeMind.Decode exposing (..)

import Parser exposing (Parser)
import Xml.Decode exposing (namedNode)


type alias Map =
    List Node


type alias Node =
    { id : String
    , text : String
    }


map : Parser Char Map
map =
    namedNode "map"


decodeMap : String -> Maybe Map
decodeMap =
    map >> Result.toMaybe
