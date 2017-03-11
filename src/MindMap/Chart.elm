module MindMap.Chart exposing (..)

import Animation
import FreeMind.Decode exposing (Node)
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (attribute, height, id, name, style, width)
import LangUtils exposing (cssSize)
import MindMap.Core exposing (Model)


type Msg
    = Init
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Init ->
            let
                res =
                    FreeMind.Decode.decodeMap model.mapText

                map =
                    if String.length model.mapText == 0 then
                        Result.Err MindMap.Core.msgNoMapData
                    else
                        Result.fromMaybe "Invalid mind map data." res
            in
                { model | map = map } ! []

        Animate time ->
            ( { model
                | animState = List.map (Animation.update time) model.animState
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.map of
        Err reason ->
            text reason

        Ok map ->
            Html.node "mindmap-chart"
                []
                [ text <| "freemind version: " ++ map.version
                , br [] []
                , div
                    [ cssSize model.width model.height
                    , style [ ( "background", "grey" ) ]
                    ]
                    (renderNodes map.nodes)
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
