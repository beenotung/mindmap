module FreeMind.Decode exposing (..)

import LangUtils
import Parser exposing (Parser)
import Xml.Decode exposing (namedNode, findAttr)


type alias Map =
    List Node


{-| Node id text children
-}
type Node
    = Node String String (List Node)


sampleRawString =
    "<map version=\"0.7.1\"><node ID=\"1\" TEXT=\"test\"><node ID=\"3\" TEXT=\"part&#x20;one\"><node ID=\"4\" TEXT=\"detail&#x20;1\"></node><node ID=\"5\" TEXT=\"detail&#x20;2\"></node></node><node ID=\"6\" TEXT=\"part&#x20;two\"></node></node></map>"


decodeMap : String -> Maybe Map
decodeMap rawString =
    map.parse (String.toList rawString)
        |> Result.toMaybe
        |> Maybe.map .head
        |> Maybe.map LangUtils.fst


map : Parser Char Map
map =
    namedNode "map"
        |> Parser.tryMap transformMap_helper


transformMap_helper : Xml.Decode.Node -> Result String Map
transformMap_helper node =
    transformMap node
        |> Result.fromMaybe "Failed to parse as FreeMind map."


transformMap : Xml.Decode.Node -> Maybe Map
transformMap node =
    case node of
        Xml.Decode.Node name attrs children ->
            if name == "map" then
                let
                    kids =
                        List.filterMap transformNode children
                in
                    if LangUtils.isSameLength kids children then
                        Just kids
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
