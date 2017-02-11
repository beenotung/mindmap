module NonEmptyList exposing (..)


type alias NonEmptyList a =
    { head : a
    , tail : List a
    }


singleton a =
    { head = a, tail = [] }


toList a =
    (a.head :: a.tail)


isSingle a =
    a.tail == []


toMaybeSingle a =
    if isSingle a then
        Just a.head
    else
        Nothing
