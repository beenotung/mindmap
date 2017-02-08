module Xml.Decode exposing (..)

import Json.Decode exposing (decodeString)


type alias Attr =
    { name : String
    , value : String
    }


type Decoder a
    = Decoder


type alias Node =
    { tag : String
    , attr : List Attr
    }


decodeXml : String -> List Node
decodeXml raw =
    []
