module LangUtils
    exposing
        ( isOk
        , isErr
        , partitionResults
        , find
        , isInRange
        , fst
        , snd
        , const
        , (>>>)
        , startWith
        , notStartWith
        , isEnglishChar
        , isSameLength
        )

import Char


isOk res =
    case res of
        Ok _ ->
            True

        _ ->
            False


isErr =
    isOk >> not


partitionResults : List (Result e a) -> ( List e, List a )
partitionResults =
    partitionResults_acc [] []


partitionResults_acc : List e -> List a -> List (Result e a) -> ( List e, List a )
partitionResults_acc errs oks res =
    case res of
        [] ->
            ( List.reverse errs, List.reverse oks )

        x :: xs ->
            case x of
                Ok v ->
                    partitionResults_acc errs (v :: oks) xs

                Err v ->
                    partitionResults_acc (v :: errs) oks xs


isInRange low high target =
    (low <= target) && (target <= high)


fst ( a, _ ) =
    a


snd ( _, b ) =
    b


const a _ =
    a


infixl 9 >>>
(>>>) : (a -> b) -> (b -> c) -> (c -> d) -> (a -> d)
(>>>) f1 f2 f3 =
    f1 >> f2 >> f3


startWith : List a -> List a -> Bool
startWith pattern target =
    case pattern of
        [] ->
            True

        x :: xs ->
            case target of
                [] ->
                    False

                y :: ys ->
                    if x == y then
                        startWith xs ys
                    else
                        False


notStartWith p t =
    not (startWith p t)


isEnglishChar c =
    Char.isLower c || Char.isUpper c


find : (a -> Bool) -> List a -> Maybe a
find p xs =
    List.filter p xs
        |> List.head


isSameLength : List a -> List b -> Bool
isSameLength a b =
    List.length a == List.length b
