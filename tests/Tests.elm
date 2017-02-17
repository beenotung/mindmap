module Tests exposing (..)

import LangUtils
import NonEmptyList
import Test exposing (..)
import Expect
import Fuzz
import String
import Parser exposing (any, element, satisfy, some, tryParse, tryParseString, int, float)


sample : Test
sample =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            , test "This test should fail - you should remove it" <|
                \() ->
                    Expect.fail "Failed as expected!"
            ]
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (Fuzz.list Fuzz.int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (Fuzz.list Fuzz.int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } Fuzz.int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 Fuzz.string Fuzz.string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]


all : Test
all =
    describe "MindMap Test Suite"
        [ describe "Library Test Suite"
            [ describe "LangUtils"
                [ test "LangUtils.startWith" <|
                    \() ->
                        Expect.true "the pattern should be matched"
                            (LangUtils.startWith [ 1, 2, 3 ] [ 1, 2, 3 ])
                , test "LangUtils.startWith" <|
                    \() ->
                        Expect.true "the pattern should be matched"
                            (LangUtils.startWith [ 1, 2, 3 ] [ 1, 2, 3, 4 ])
                , test "LangUtils.startWith" <|
                    \() ->
                        Expect.false "the pattern should not be matched"
                            (LangUtils.startWith [ 1, 3 ] [ 1, 2, 3 ])
                ]
            , describe "Parser Test Suite"
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
                            {-
                               input           : 12\"3
                               expected output : 12"3  ,   <>
                               real output     : 12\   ,   3"
                            -}
                            (tryParseString "\"12\\\"3\"" Parser.quotedString)
                            (Ok ( "12\"3", "" ))
                ]
            ]
        ]
