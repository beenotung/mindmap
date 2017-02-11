module Parser exposing (..)

import Basics exposing (Never, never)


type alias Parser c a =
    { parse : List c -> List ( a, List c ) }


parse : Parser c a -> List c -> List ( a, List c )
parse p =
    p.parse


runParser : Parser c a -> List c -> Result String a
runParser p cs =
    case p.parse cs of
        [ ( a, [] ) ] ->
            Ok a

        [ ( _, rs ) ] ->
            Err "Parser did not consume entire stream."

        _ ->
            Err "Parser error."


item : Parser c c
item =
    { parse =
        \cs ->
            case cs of
                [] ->
                    []

                c :: cs ->
                    [ ( c, cs ) ]
    }


bind : Parser c a -> (a -> Parser c b) -> Parser c b
bind p f =
    { parse = p.parse >> List.concatMap (\( a, s ) -> (f a).parse s) }


success : a -> Parser c a
success a =
    { parse = \cs -> [ ( a, cs ) ] }


failure : Parser c a
failure =
    { parse = \_ -> [] }


combine : Parser c a -> Parser c a -> Parser c a
combine p q =
    { parse = \cs -> p.parse cs ++ q.parse cs }


{-| Try to parse if the predicate satisfy.

    (==) 1 >> satisfy >> parse [1,2,3]    == [(1, [2,3])]
    (==) 2 >> satisfy >> parse [1,2,3]    == []
-}
satisfy : (c -> Bool) -> Parser c c
satisfy p =
    bind item
        (\c ->
            if p c then
                success c
            else
                failure
        )


{-| Take one given element from the stream.

    runParser (element 'c') ['a', 'b', 'c'] == Err ...
    runParser (element 'a') ['a', 'b', 'c'] == Ok 'a'
-}
element : c -> Parser c c
element c =
    satisfy ((==) c)



--{-| One or more.
--
--    (==) 1 >> satisfy >> any >> parse [0,1,2]   == []
--    (==) 1 >> satisfy >> any >> parse [1,2,3]   == [([1], [2,3])]
--    (==) 1 >> satisfy >> any >> parse [1,1,2]   == [([1,1], [2])]
---}
--some : Parser c a -> Parser c (List a)
--some p =
--    { parse =
--        \cs ->
--            case p.parse of
--                [ ( a, rs ) ] ->
--                    ( [ a ], rs )
--    }


{-| Zero or more.

    (==) 1 >> satisfy >> any >> parse [0,1,2]   == [([], [0,1,2])]
    (==) 1 >> satisfy >> any >> parse [1,2,3]   == [([1], [2,3])]
    (==) 1 >> satisfy >> any >> parse [1,1,2]   == [([1,1], [2])]
-}
any : Parser c a -> Parser c (List a)
any p =
    { parse = p.parse >> List.map (\( a, rs ) -> ( [ a ], rs ))
    }
