module Main exposing (main)

import Debug exposing (log)
import Html exposing (Html, body, h1, text)
import Html.Attributes exposing (style)
import LangUtils
import MindMap.Container


type alias Model =
    { title : String, mindMap : MindMap.Container.Model }


type alias Msg =
    MindMap.Container.Msg


initModel : Model
initModel =
    { title = "MindMap"
    , mindMap = MindMap.Container.initModel
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        ( newModel, cmd ) =
            MindMap.Container.update msg model.mindMap
    in
        ( { model | mindMap = newModel }, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    body [ bodyCss ]
        [ h1 [] [ text model.title ]
        , MindMap.Container.view model.mindMap
        ]


bodyCss =
    style [ ( "margin", "8px" ) ]


main =
    Html.program
        { init = initModel ! []
        , update = LangUtils.logFunction "update" update
        , subscriptions = subscriptions
        , view = view
        }
