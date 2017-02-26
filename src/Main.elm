module Main exposing (main)

import Debug exposing (log)
import Html exposing (Html, body, h1, text)
import Html.Attributes exposing (style)
import LangUtils
import MindMap.Container
import MindMap.Core


type alias Model =
    { title : String, mindMap : MindMap.Core.Model }


type alias Msg =
    MindMap.Container.Msg


initModel : Model
initModel =
    { title = "MindMap"
    , mindMap = MindMap.Core.initModel
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    MindMap.Container.update msg model.mindMap
        |> \x -> { model | mindMap = x } ! []


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
