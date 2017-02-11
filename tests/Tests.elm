module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Parser exposing (element, tryParse, satisfy)


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
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]


all : Test
all =
    describe "MindMap Test Suite"
        [ describe "Library Test Suite"
            [ describe "Parser Test Suite"
                [ fuzzWith { runs = 1000 } int "random integer test" <|
                    \i -> i > i - 1 |> Expect.true "Integer overflow?!"
                , test "Parser.satisfy" <|
                    \() ->
                        Expect.equal
                            (tryParse [ 1, 2, 3 ] (satisfy (\x -> x > 0)))
                            (Ok ( 1, [ 2, 3 ] ))
                , test "Parser.element" <|
                    \() ->
                        Expect.true "This parser should fail."
                            (tryParse [ 1, 2, 3 ] (element 0) |> (==) (Err [ 1, 2, 3 ]))
                , test "Parser.element" <|
                    \() ->
                        Expect.true "This parser should not fail."
                            (tryParse [ 1, 2, 3 ] (element 1) |> (==) (Ok ( 1, [ 2, 3 ] )))
                ]
            ]
        ]
