module MindMap.Chart exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (name)
import MindMap.Core


type alias Model =
    Maybe MindMap.Core.MindMap


type Msg
    = Msg


initModel : Model
initModel =
    Nothing


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    model ! []


view : Model -> Html Msg
view model =
    div [ name "MindMap.Chart" ] []
