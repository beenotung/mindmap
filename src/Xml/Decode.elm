module Xml.Decode exposing (..)

import Json.Decode exposing (decodeString)


type alias Attr =
    { name : String
    , value : String
    }


type Decoder a
    = Decoder


type alias Node =
    { tag : String
    , attr : List Attr
    }


decodeXml : String -> List Node
decodeXml raw =
    []



--decodeString : List Char -> List String
--decodeString
--node : String -> Result String Node


type alias Parser a =
    { parse : List Char -> List ( a, List Char ) }


runParser : Parser a -> List Char -> Result String a
runParser m s =
    case m.parse s of
        [ ( res, [] ) ] ->
            Ok res

        [ ( _, rs ) ] ->
            Err "Parser did not consume entire stream."

        _ ->
            Err "Parser error."


item : Parser Char
item =
    { parse =
        \s ->
            case s of
                [] ->
                    []

                c :: cs ->
                    [ ( c, cs ) ]
    }


bind : Parser a -> (a -> Parser b) -> Parser b
bind p f =
    { parse = p.parse >> List.concatMap (\( a, s1 ) -> (f a).parse s1) }


unit : a -> Parser a
unit a =
    { parse = \cs -> [ ( a, cs ) ] }


failure : Parser a
failure =
    { parse = \_ -> [] }


combine : Parser a -> Parser a -> Parser a
combine p q =
    { parse = \s -> p.parse s ++ q.parse s }



--option::


satisfy : (Char -> Bool) -> Parser Char
satisfy p =
    bind item
        (\c ->
            if p c then
                unit c
            else
                failure
        )


char : Char -> Parser Char
char c =
    satisfy ((==) c)



--try : Parser a -> Parser (Maybe a)
--try p =
--    { parse =
--        \s ->
--            case p.parse s of
--                [] ->
--                    Nothing
--
--                xs ->
--                    Just xs
--    }
-- | One or more.
--some : Parser a -> Parser (List a)
--some p s = case p.parse s of
--  [] -> []
--  [(a,rs)] -> [(a,)]
-- | Zero or more.
--many : Parser a -> Parser (List a)
--many p =
--    { parse =
--        \s ->
--            List.concatMap
--              ()
--              (p.parse s)
--            case p.parse s of
--                [] ->
--                    []
--
--                [ ( a, rs ) ] ->
--                    []
--    }
--
