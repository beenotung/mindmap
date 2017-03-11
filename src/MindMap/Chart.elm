module MindMap.Chart exposing (..)

import Animation
import Dict exposing (Dict)
import FreeMind.Decode exposing (FreeMind, Node)
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (attribute, height, id, name, style, width)
import LangUtils exposing (cssSize)
import MindMap.Core exposing (Model, Position)


type Msg
    = Init
    | Animate Animation.Msg


initNodePosition : List Node -> Dict FreeMind.Decode.Node Position
initNodePosition nodes =
    Dict.empty


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Init ->
            let
                mindMap =
                    if String.length model.mapText == 0 then
                        Result.Err MindMap.Core.msgNoMapData
                    else
                        FreeMind.Decode.decodeMap model.mapText
                            |> Maybe.map
                                (\mapData ->
                                    { mapData = mapData
                                    , nodePositionDict = initNodePosition mapData.nodes
                                    }
                                )
                            |> Result.fromMaybe "Invalid mind map data."
            in
                { model | mindMap = mindMap } ! []

        Animate time ->
            ( { model
                | animState = List.map (Animation.update time) model.animState
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.mindMap of
        Err reason ->
            text reason

        Ok mindMap ->
            Html.node "mindmap-chart"
                []
                [ text <| "freemind version: " ++ mindMap.mapData.version
                , br [] []
                , div
                    [ cssSize model.width model.height
                    , style [ ( "background", "grey" ) ]
                    ]
                    (renderNodes mindMap.nodePositionDict)
                ]


renderNode : ( Node, Position ) -> Html Msg
renderNode ( node, position ) =
    case node of
        FreeMind.Decode.Node id name children ->
            Html.node "mindmap-node"
                []
                [ text name
                , text <| toString position.x
                , text <| toString position.y
                ]


{-|
 TODO read http://reasonablypolymorphic.com/blog/elm-is-wrong
-}
renderNodes : Dict Node Position -> List (Html Msg)
renderNodes nodePositionDict =
    List.map renderNode (Dict.toList nodePositionDict)
