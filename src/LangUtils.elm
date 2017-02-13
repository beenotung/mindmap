module LangUtils exposing (isOk, isErr, partitionResults)


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
