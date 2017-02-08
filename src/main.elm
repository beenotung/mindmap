module Main exposing (..)

import Html exposing (beginnerProgram, text)


init =
    "init"


view model =
    text model


update msg model =
    model


main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }
