module MindMap.Core exposing (..)


type alias MindMap =
    { version : String
    , nodes : List Node
    }


type Node
    = Node NodeMeta (List NodeMeta)


type alias NodeMeta =
    { id : String, text : String }
