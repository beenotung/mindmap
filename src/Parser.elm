module Parser exposing (..)

import NonEmptyList exposing (NonEmptyList)


{-| Generic building block for parser. e.g. XMLDecoder, Compiler, Interpreter

# Definition
@docs Parser

-}
type alias Parser c a =
    { parse : List c -> Result String (NonEmptyList ( a, List c )) }


fst ( a, _ ) =
    a


parse : List c -> Parser c a -> Result String a
parse cs p =
    case p.parse cs of
        Err reason ->
            Err reason

        Ok res ->
            Ok (fst res.head)


tryParse : List c -> Parser c a -> Result (List c) ( a, List c )
tryParse cs p =
    case p.parse cs of
        Err reason ->
            Err cs

        Ok res ->
            Ok res.head


item : Parser c c
item =
    { parse =
        \cs ->
            case cs of
                [] ->
                    Err "Cannot take item from empty stream."

                c :: cs ->
                    Ok (NonEmptyList.singleton ( c, cs ))
    }


success : a -> Parser c a
success a =
    { parse = \cs -> Ok (NonEmptyList.singleton ( a, cs )) }


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
                    case NonEmptyList.toMaybeSingle res of
                        Just ( a, rs ) ->
                            (f a).parse rs

                        Nothing ->
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
