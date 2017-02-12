module Parser exposing (..)

import Char
import NonEmptyList exposing (NonEmptyList)


{-| Generic building block for parser. e.g. XMLDecoder, Compiler, Interpreter

# Definition
@docs Parser

-}
type alias Parser c a =
    { parse : List c -> Result String (NonEmptyList ( a, List c )) }


xorResult a b =
    case ( a, b ) of
        ( Ok _, Ok _ ) ->
            Err "xorResult: Both result are Ok"

        ( Err _, Err _ ) ->
            Err "xorResult: Both result are Err"

        ( Ok a, _ ) ->
            Ok a

        ( _, Ok a ) ->
            Ok a


orResult a b =
    case ( a, b ) of
        ( Ok a, _ ) ->
            Ok a

        ( _, Ok a ) ->
            Ok a

        ( Err a, _ ) ->
            Err a


combineResult : Result String (NonEmptyList ( a, List c )) -> Result String (NonEmptyList ( a, List c )) -> Result String (NonEmptyList ( a, List c ))
combineResult a b =
    case ( a, b ) of
        ( Err x, Err y ) ->
            Err ("Both result failed:\n\t" ++ x ++ "\n\t" ++ y)

        ( Ok x, Ok y ) ->
            Ok (NonEmptyList.append x y)

        ( Ok x, _ ) ->
            Ok x

        ( _, Ok y ) ->
            Ok y


fst ( a, _ ) =
    a


snd ( _, b ) =
    b


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


tryParseString : String -> Parser Char a -> Result String ( a, String )
tryParseString s p =
    case p.parse (String.toList s) of
        Err reason ->
            Err s

        Ok res ->
            let
                ( a, rs ) =
                    res.head
            in
                Ok ( a, String.fromList rs )


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


fmap : (a -> b) -> Parser c a -> Parser c b
fmap f p =
    bind p
        (\a ->
            let
                b =
                    f a
            in
                success b
        )


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


{-| Zero or more.
-}
any : Parser c a -> Parser c (List a)
any p =
    { parse =
        \cs -> Ok (NonEmptyList.singleton (any_acc [] p cs))
    }


any_acc : List a -> Parser c a -> List c -> ( List a, List c )
any_acc acc p cs =
    case tryParse cs p of
        Err rs ->
            ( acc, rs )

        Ok ( a, rs ) ->
            any_acc (a :: acc) p rs


{-| One or More.
-}
some : Parser c a -> Parser c (NonEmptyList a)
some p =
    { parse =
        \cs ->
            case tryParse cs p of
                Err rs ->
                    Err "`Some` Parser failed to find at least one pattern."

                Ok ( a, rs ) ->
                    let
                        ( as_, rs2 ) =
                            any_acc [] p rs

                        nonEmptyAs =
                            { head = a, tail = as_ }
                    in
                        Ok (NonEmptyList.singleton ( nonEmptyAs, rs2 ))
    }


space =
    element ' '


{-| Skip all spaces, zero or more. Count number of space collected
-}
spaces : Parser Char Int
spaces =
    fmap List.length (any space)


option : Parser c a -> Parser c a -> Parser c a
option p q =
    { parse =
        \cs ->
            case tryParse cs p of
                Ok res ->
                    Ok (NonEmptyList.singleton res)

                Err rs ->
                    case tryParse cs q of
                        Ok res ->
                            Ok (NonEmptyList.singleton res)

                        Err rs ->
                            Err "Both option failed."
    }


options : NonEmptyList (Parser c a) -> Parser c a
options ps =
    options_acc ps.head ps.tail


options_acc : Parser c a -> List (Parser c a) -> Parser c a
options_acc acc ps =
    case ps of
        [] ->
            acc

        x :: xs ->
            options_acc (option acc x) xs


combine : Parser c a -> Parser c a -> Parser c a
combine p q =
    { parse = \cs -> combineResult (p.parse cs) (q.parse cs) }


digit : Parser Char Int
digit =
    NonEmptyList.wrap 0 (List.range 1 9)
        |> NonEmptyList.map (\i -> Char.fromCode (48 + i))
        |> NonEmptyList.map element
        |> NonEmptyList.reduce option
        |> fmap Char.toCode
        |> fmap ((-) 48)



--
--
--int : Parser Char Int
--int =
--    item
