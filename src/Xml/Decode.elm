module Xml.Decode exposing (..)

import Parser exposing (Parser, (<*), (*>), (>>=))


type alias Attr =
    { name : String
    , value : String
    }


type alias Node =
    { tag : String
    , attr : List Attr
    }


type alias NodeRecord =
    { parent : Node
    , children : List Node
    }


nodeHead : Parser Char String
nodeHead =
    Parser.char '<'
        *> Parser.englishWord
        <* Parser.char '>'


nodeTail : Parser Char String
nodeTail =
    Parser.char '<'
        *> Parser.char '/'
        *> Parser.englishWord
        <* Parser.char '>'


attr : Parser Char Attr
attr =
    Parser.englishWord
        <* Parser.char '='
        |> Parser.flippedChain Parser.quotedString
        |> Parser.map
            (\( a, b ) ->
                { name = a
                , value = b
                }
            )
