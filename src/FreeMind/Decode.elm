module FreeMind.Decode exposing (..)

import LangUtils
import Parser exposing (Parser)
import Xml.Decode exposing (namedNode, findAttr)


type alias FreeMind =
    { version : String
    , nodes : List Node
    }


{-| Node id text children
-}
type Node
    = Node String String (List Node)


sampleRawString =
    "<map version=\"0.7.1\"><node ID=\"1\" TEXT=\"test\"><node ID=\"3\" TEXT=\"part&#x20;one\"><node ID=\"4\" TEXT=\"detail&#x20;1\"></node><node ID=\"5\" TEXT=\"detail&#x20;2\"></node></node><node ID=\"6\" TEXT=\"part&#x20;two\"></node></node></map>"


decodeMap : String -> Maybe FreeMind
decodeMap rawString =
    map.parse (String.toList rawString)
        |> Result.toMaybe
        |> Maybe.map .head
        |> Maybe.map LangUtils.fst


map : Parser Char FreeMind
map =
    namedNode "map"
        |> Parser.tryMap transformMap_helper


transformMap_helper : Xml.Decode.Node -> Result String FreeMind
transformMap_helper node =
    transformMap node
        |> Result.fromMaybe "Failed to parse as FreeMind map."


transformMap : Xml.Decode.Node -> Maybe FreeMind
transformMap node =
    case node of
        Xml.Decode.Node name attrs children ->
            if name == "map" then
                let
                    kids =
                        List.filterMap transformNode children
                in
                    if LangUtils.isSameLength kids children then
                        Xml.Decode.findAttr "version" attrs
                            |> Maybe.map
                                (\version ->
                                    { version = version, nodes = kids }
                                )
                    else
                        Nothing
            else
                Nothing


node : Parser Char Node
node =
    Parser.bind Xml.Decode.node
        (\node ->
            transformNode node
                |> Maybe.map Parser.success
                |> Maybe.withDefault (Parser.fail "Missing ID or TEXT in node.")
        )


transformNode : Xml.Decode.Node -> Maybe Node
transformNode node =
    case node of
        Xml.Decode.Node name attrs children ->
            let
                id =
                    findAttr "ID" attrs

                text =
                    findAttr "TEXT" attrs

                childrenNode =
                    List.filterMap transformNode children
            in
                if List.length children == List.length childrenNode then
                    Maybe.map2 (curry (\( id, text ) -> Node id text childrenNode)) id text
                else
                    Nothing
