module Parser exposing (..)

{-| Generic building block for parser. e.g. XMLDecoder, Compiler, Interpreter

# Definition
@docs Parser

-}


type alias Parser c a =
    { parse : List c -> Result String (List ( a, List c )) }


parse : List c -> Parser c a -> Result String a
parse cs p =
    case p.parse cs of
        Err reason ->
            Err reason

        Ok res ->
            case res of
                [ ( a, [] ) ] ->
                    Ok a

                [ _ ] ->
                    Err "Parser did not consume entire stream."

                _ ->
                    Err "Parser error."


tryParse : List c -> Parser c a -> Result (List c) ( a, List c )
tryParse cs p =
    case p.parse cs of
        Err reason ->
            Err cs

        Ok res ->
            case res of
                [ ( a, rs ) ] ->
                    Ok ( a, rs )

                _ ->
                    Err cs


item : Parser c c
item =
    { parse =
        \cs ->
            case cs of
                [] ->
                    Err "The stream is empty."

                c :: cs ->
                    Ok [ ( c, cs ) ]
    }


success : a -> Parser c a
success a =
    { parse = \cs -> Ok [ ( a, cs ) ] }


fail : String -> Parser c a
fail reason =
    { parse = \cs -> Err reason }


bind : Parser c a -> (a -> Parser c b) -> Parser c b
bind p f =
    { parse =
        \cs ->
            case p.parse cs of
                Err reason ->
                    Err reason

                Ok res ->
                    case res of
                        [ ( a, rs ) ] ->
                            (f a).parse rs

                        _ ->
                            Err "Parser error."
    }


satisfy : (a -> Bool) -> Parser a a
satisfy p =
    bind item
        (\a ->
            if p a then
                success a
            else
                fail "Predicate not satisfied."
        )


element : a -> Parser a a
element a =
    (==) a |> satisfy
