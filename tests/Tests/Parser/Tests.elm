module Tests.Parser.Tests exposing (all)

import Expect
import Fuzz
import NonEmptyList
import Parser exposing (any, element, float, int, satisfy, some, tryParse, tryParseString)
import Test exposing (describe, fuzzWith, test)


all =
    describe "Parser Test Suite"
        [ fuzzWith { runs = 1000 } Fuzz.int "random integer test" <|
            \i -> i > i - 1 |> Expect.true "Integer overflow?!"
        , test "Parser.satisfy" <|
            \() ->
                Expect.equal
                    (tryParse [ 1, 2, 3 ] (satisfy (\x -> x > 0)))
                    (Ok ( 1, [ 2, 3 ] ))
        , test "Parser.element" <|
            \() ->
                Expect.equal
                    (tryParse [ 1, 2, 3 ] (element 0))
                    (Err [ 1, 2, 3 ])
        , test "Parser.element" <|
            \() ->
                Expect.equal
                    (tryParse [ 1, 2, 3 ] (element 1))
                    (Ok ( 1, [ 2, 3 ] ))
        , test "Parser.any" <|
            \() ->
                Expect.equal
                    (tryParse [ 1, 2, 3 ] (element 1 |> any))
                    (Ok ( [ 1 ], [ 2, 3 ] ))
        , test "Parser.any" <|
            \() ->
                Expect.equal
                    (tryParse [ 1, 1, 2 ] (element 1 |> any))
                    (Ok ( [ 1, 1 ], [ 2 ] ))
        , test "Parser.some" <|
            \() ->
                Expect.equal
                    (tryParse [ 1, 2, 3 ] (element 0 |> some))
                    (Err [ 1, 2, 3 ])
        , test "Parser.some" <|
            \() ->
                Expect.equal
                    (tryParse [ 1, 2, 3 ] (element 1 |> some))
                    (Ok ( NonEmptyList.singleton 1, [ 2, 3 ] ))
        , test "Parser.some" <|
            \() ->
                Expect.equal
                    (tryParse [ 1, 1, 2 ] (element 1 |> some))
                    (Ok ( NonEmptyList.wrap 1 [ 1 ], [ 2 ] ))
        , test "Parser.tryParseString" <|
            \() ->
                Expect.equal
                    (tryParseString "abc" (element 'a' |> any))
                    (Ok ( [ 'a' ], "bc" ))
        , test "Parser.int" <|
            \() ->
                Expect.equal
                    (tryParseString "12345" int)
                    (Ok ( 12345, "" ))
        , test "Parser.int" <|
            \() ->
                Expect.equal
                    (tryParseString "012345" int)
                    (Ok ( 12345, "" ))
        , test "Parser.int" <|
            \() ->
                Expect.equal
                    (tryParseString "0.12345" int)
                    (Ok ( 0, ".12345" ))
        , test "Parser.int" <|
            \() ->
                Expect.equal
                    (tryParseString ".12345" int)
                    (Err "`some` Parser failed to find at least one pattern.")
        , test "Parser.float" <|
            \() ->
                Expect.equal
                    (tryParseString "1" float)
                    (Ok ( 1, "" ))
        , test "Parser.float" <|
            \() ->
                Expect.equal
                    (tryParseString "1.2" float)
                    (Ok ( 1.2, "" ))
        , test "Parser.float" <|
            \() ->
                Expect.equal
                    (tryParseString ".1" float)
                    (Err "Missing digit for float.")
        , test "Parser.float" <|
            \() ->
                Expect.equal
                    (tryParseString "a" float)
                    (Err "Missing digit for float.")
        , test "Parser.quotedString" <|
            \() ->
                Expect.equal
                    (tryParseString "123" Parser.quotedString)
                    (Err "Cannot find a pair of separatorChar.")
        , test "Parser.quotedString" <|
            \() ->
                Expect.equal
                    (tryParseString "\"123\"" Parser.quotedString)
                    (Ok ( "123", "" ))
        , test "Parser.quotedString" <|
            \() ->
                Expect.equal
                    (tryParseString "\"12\\\"3\"" Parser.quotedString)
                    (Ok ( "12\"3", "" ))
        , test "Parser.englishWord" <|
            \() ->
                Expect.equal
                    (tryParseString "word" Parser.englishWord)
                    (Ok ( "word", "" ))
        , test "Parser.englishWord" <|
            \() ->
                Expect.equal
                    (tryParseString "word>" Parser.englishWord)
                    (Ok ( "word", ">" ))
        ]
