module MindMap.Chart exposing (..)

import FreeMind.Decode exposing (Node)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id, name)
import MindMap.Core exposing (Model)


type Msg
    = Init


update : Msg -> Model -> Model
update msg model =
    case msg of
        Init ->
            let
                map =
                    FreeMind.Decode.decodeMap model.mapText
            in
                { model | map = map }


view : Model -> Html Msg
view model =
    case model.map of
        Nothing ->
            text "No mind map data yet."

        Just map ->
            Html.node "mindmap-chart"
                []
                [ text <| "freemind version: " ++ map.version
                , div [] (renderNodes map.nodes)
                ]


renderNode : Node -> Html Msg
renderNode node =
    case node of
        FreeMind.Decode.Node id name children ->
            Html.node "mindmap-node"
                []
                [ text name
                , div [] (renderNodes children)
                ]


renderNodes : List Node -> List (Html Msg)
renderNodes =
    List.map renderNode
