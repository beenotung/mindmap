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


char : Char -> Parser Char Char
char =
    Parser.element


nodeHead : Parser Char String
nodeHead =
    char '<'
        *> Parser.englishWord
        <* char '>'


nodeTail : Parser Char String
nodeTail =
    char '<'
        *> char '/'
        *> Parser.englishWord
        <* char '>'


attr : Parser Char Attr
attr =
    Parser.englishWord
        <* char '='
        |> Parser.flippedChain Parser.quotedString
        |> Parser.map
            (\( a, b ) ->
                { name = a
                , value = b
                }
            )
