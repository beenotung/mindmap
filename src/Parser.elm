module Parser exposing (..)

import Char
import NonEmptyList exposing (NonEmptyList)
import LangUtils exposing (isOk, isErr, const)


infixl 0 *>


infixl 0 <*


{-| Generic building block for parser. e.g. XMLDecoder, Compiler, Interpreter

# Definition
@docs Parser

-}
type alias Parser c a =
    { parse : List c -> ParserResult c a }


type alias ParserResult c a =
    Result String (NonEmptyList ( a, List c ))


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


parse : List c -> Parser c a -> Result String a
parse cs p =
    case p.parse cs of
        Err reason ->
            Err reason

        Ok res ->
            Ok (LangUtils.fst res.head)


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
            Err reason

        Ok res ->
            let
                ( a, rs ) =
                    res.head
            in
                Ok ( a, String.fromList rs )


{-| will never fail
-}
withDefault : a -> Parser c a -> Parser c a
withDefault a p =
    { parse =
        \cs ->
            case p.parse cs of
                Err _ ->
                    Ok (NonEmptyList.singleton ( a, cs ))

                Ok res ->
                    Ok res
    }


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


(>>=) : (a -> Parser c b) -> Parser c a -> Parser c b
(>>=) =
    flip bind


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


flatMap =
    bind


map : (a -> b) -> Parser c a -> Parser c b
map f p =
    bind p
        (\a ->
            let
                b =
                    f a
            in
                success b
        )


mapError : (String -> String) -> Parser c a -> Parser c a
mapError f p =
    { parse = p.parse >> Result.mapError f }


replaceError : String -> Parser c a -> Parser c a
replaceError =
    LangUtils.const >> mapError


satisfy : (a -> Bool) -> Parser a a
satisfy p =
    bind item
        (\a ->
            if p a then
                success a
            else
                fail "Predicate not satisfied."
        )


{-| Similar to satisfy, but apply to the stream.
-}
check : String -> (List c -> Bool) -> Parser c a -> Parser c a
check errorMsg pred parser =
    { parse =
        \cs ->
            if pred cs then
                parser.parse cs
            else
                Err errorMsg
    }


element : a -> Parser a a
element a =
    (==) a |> satisfy


char : Char -> Parser Char Char
char =
    element


elementSeq : List c -> Parser c (List c)
elementSeq cs =
    case cs of
        [] ->
            success []

        x :: xs ->
            chain
                (element x)
                (elementSeq xs)
                |> map (const cs)


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
            ( List.reverse acc, rs )

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
                    Err "`some` Parser failed to find at least one pattern."

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
    map List.length (any space)


{-| Alternative.

If the first parser success, return the result.
Otherwise return the result of second parser.
-}
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


{-| concat the result of both parsers
-}
combine : Parser c a -> Parser c a -> Parser c a
combine p q =
    { parse = \cs -> combineResult (p.parse cs) (q.parse cs) }



{- TODO -}


optional : Parser c a -> Parser c (Maybe a)
optional p =
    { parse =
        \cs ->
            case p.parse cs of
                Err _ ->
                    Ok (NonEmptyList.singleton ( Nothing, cs ))

                Ok res ->
                    res
                        |> NonEmptyList.map (\( a, rs ) -> ( Just a, rs ))
                        |> Ok
    }


{-| Pipe the require of parser to the second parser. (a.k.a. andThen)
-}
chain : Parser c a -> Parser c b -> Parser c ( a, b )
chain p q =
    bind p
        (\a ->
            bind q
                (\b ->
                    success ( a, b )
                )
        )


{-| flipped chain.
-}
pairWith : Parser c b -> Parser c a -> Parser c ( a, b )
pairWith =
    flip chain


andThen =
    chain


fst : Parser c ( a, b ) -> Parser c a
fst =
    map LangUtils.fst


snd : Parser c ( a, b ) -> Parser c b
snd =
    map LangUtils.snd


(*>) : Parser c a -> Parser c b -> Parser c b
(*>) p q =
    chain p q
        |> snd


(<*) : Parser c a -> Parser c b -> Parser c a
(<*) p q =
    chain p q |> fst


digit : Parser Char Int
digit =
    satisfy Char.isDigit
        |> map (\c -> Char.toCode c - Char.toCode '0')


int : Parser Char Int
int =
    let
        int_acc acc c =
            acc * 10 + c
    in
        some digit
            |> map (NonEmptyList.reduce int_acc)


{-| Parse Float from Char.

    valid   : 1
    valid   : 1.2

    invalid : .1

    Example : "1.a" ~~> (1, ".a")

    self : p1 ++ optional(p2)
    p1   : int (required, not optional)
    p2   : if '.' exist,
         then parse int then (int->float with offset)
         else fail
-}
float : Parser Char Float
float =
    chain
        float_helper_1
        (withDefault 0 float_helper_2)
        |> map (uncurry (+))


{-| Before decimal place.
-}
float_helper_1 : Parser Char Float
float_helper_1 =
    int
        |> map toFloat
        |> replaceError "Missing digit for float."


{-| The decimal place.
-}
float_helper_2 : Parser Char Float
float_helper_2 =
    chain (element '.') (some digit)
        |> map
            (\( _, list ) ->
                float_helper_3 (toFloat list.head / 10) (1 / 100) list.tail
            )


{-| After decimal place.
-}
float_helper_3 : Float -> Float -> List Int -> Float
float_helper_3 acc offset list =
    case list of
        [] ->
            acc

        x :: xs ->
            float_helper_3 (acc + (toFloat x) * offset) (offset / 10) xs


string : String -> Parser Char String
string content =
    content
        |> String.toList
        |> elementSeq
        |> map (const content)


quotedString : Parser Char String
quotedString =
    genQuotedString '"' "\\\""


{-| generate ta string parser.

    Example : quotedString '"' "\\\"" ~> parser

    p1 : element separatorChar
    p2 : any (not $ word escapeSeq)
    p3 : element separatorChar

-}
genQuotedString : Char -> String -> Parser Char String
genQuotedString separatorChar escapeSeq =
    genQuotedSeq separatorChar (String.toList escapeSeq)
        |> map String.fromList


genQuotedSeq : c -> List c -> Parser c (List c)
genQuotedSeq separatorElement escapeSeq =
    let
        separatorParser =
            element separatorElement
    in
        chain
            separatorParser
            { parse =
                \cs ->
                    let
                        ( res, rs ) =
                            genQuotedSeq_helper [] cs separatorElement escapeSeq (List.length escapeSeq)
                    in
                        Ok (NonEmptyList.singleton ( res, rs ))
            }
            |> snd
            |> (flip chain) separatorParser
            |> fst
            |> replaceError "Cannot find a pair of separatorChar."


genQuotedSeq_helper : List c -> List c -> c -> List c -> Int -> ( List c, List c )
genQuotedSeq_helper acc stream stopElement escapeSeq escapeSeqLen =
    case stream of
        [] ->
            ( List.reverse acc, [] )

        x :: xs ->
            if LangUtils.startWith escapeSeq stream then
                {- met escape sequence, replace to the escaped element, then continue -}
                genQuotedSeq_helper (stopElement :: acc) (List.drop escapeSeqLen stream) stopElement escapeSeq escapeSeqLen
            else if x == stopElement then
                {- stop -}
                ( List.reverse acc, stream )
            else
                {- continue -}
                genQuotedSeq_helper (x :: acc) xs stopElement escapeSeq escapeSeqLen


{-| return (newAcc,restStream).
-}
genQuotedSeq_helper_takeSeq : List c -> List c -> List c -> ( List c, List c )
genQuotedSeq_helper_takeSeq acc stream pattern =
    case pattern of
        [] ->
            ( acc, stream )

        x :: xs ->
            case stream of
                [] ->
                    ( acc, [] )

                y :: ys ->
                    genQuotedSeq_helper_takeSeq (x :: acc) ys xs


englishChar : Parser Char Char
englishChar =
    satisfy LangUtils.isEnglishChar


englishWord : Parser Char String
englishWord =
    some englishChar
        |> map NonEmptyList.toList
        |> map String.fromList
