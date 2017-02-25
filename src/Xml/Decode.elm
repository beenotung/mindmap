module Xml.Decode exposing (..)

import Json.Decode
import LangUtils
import Parser exposing (Parser, (<*), (*>), (>>=))


type Decoder a
    = Decoder


type alias Attr =
    { name : String
    , value : String
    }


type Node
    = Node String (List Attr) (List Node)


node : Parser Char Node
node =
    { parse =
        \cs ->
            (nodeHead
                |> Parser.pairWith (Parser.any node |> Parser.wrapBySpaces)
                |> Parser.pairWith nodeTail
                |> (flip Parser.bind)
                    (\( ( ( headTag, attrs ), childNodes ), tailTag ) ->
                        if headTag == tailTag then
                            Parser.success
                                (Node headTag attrs childNodes)
                        else
                            Parser.fail <| "opening tag `" ++ headTag ++ "` does not match closing tag `" ++ tailTag ++ "`."
                    )
            ).parse
                cs
    }


nodeHead : Parser Char ( String, List Attr )
nodeHead =
    Parser.char '<'
        *> Parser.englishWord
        |> Parser.pairWith (Parser.any (Parser.spaces *> attr <* Parser.spaces))
        <* Parser.char '>'
        |> Parser.replaceError "Parser `nodeHead` failed."


nodeTail : Parser Char String
nodeTail =
    Parser.char '<'
        *> Parser.char '/'
        *> Parser.englishWord
        <* Parser.char '>'
        |> Parser.replaceError "Parser `nodeTail` failed."


attr : Parser Char Attr
attr =
    Parser.englishWord
        <* Parser.char '='
        |> Parser.pairWith Parser.quotedString
        |> Parser.map
            (\( a, b ) ->
                { name = a
                , value = b
                }
            )
        |> Parser.replaceError "Parser `attr` failed."


{-| shortcut helper.
-}
emptyNode : String -> Node
emptyNode name =
    Node name [] []


namedNode : String -> Parser Char Node
namedNode targetName =
    Parser.bind node
        (\node ->
            case node of
                Node name attrs children ->
                    if name == targetName then
                        Parser.success node
                    else
                        Parser.fail ("Unexpected node of name `" ++ name ++ "`, expecting name `" ++ targetName ++ "`")
        )


findAttr : String -> List Attr -> Maybe String
findAttr name attrs =
    LangUtils.find (.name >> (==) name) attrs
        |> Maybe.map .value
