module NonEmptyList exposing (..)


type alias NonEmptyList a =
    { head : a
    , tail : List a
    }


singleton a =
    { head = a, tail = [] }


toList a =
    (a.head :: a.tail)


wrap x xs =
    { head = x, tail = xs }


isSingle a =
    a.tail == []


toMaybeSingle a =
    if isSingle a then
        Just a.head
    else
        Nothing


(++) : NonEmptyList a -> NonEmptyList a -> NonEmptyList a
(++) xs ys =
    { head = xs.head
    , tail = List.append xs.tail (ys.head :: ys.tail)
    }


append =
    (++)


reduce : (a -> a -> a) -> NonEmptyList a -> a
reduce f list =
    list.head


fromList : List a -> Maybe (NonEmptyList a)
fromList list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (wrap x xs)


map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map f list =
    wrap
        (f list.head)
        (List.map f list.tail)
