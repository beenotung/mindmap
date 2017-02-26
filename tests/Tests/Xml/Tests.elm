module Tests.Xml.Tests exposing (all)

import Expect
import Parser exposing (tryParse, tryParseString)
import Test exposing (describe, test)
import Xml.Decode


all =
    describe "Xml Test Suite" [ simple, realWorld ]


simple =
    describe "Xml.Decode (Simple)"
        [ test "Xml.Decode.nodeHead" <|
            \() ->
                Expect.equal
                    (tryParseString "<map>" Xml.Decode.nodeHead)
                    (Ok ( ( "map", [] ), "" ))
        , test "Xml.Decode.nodeTail" <|
            \() ->
                Expect.equal
                    (tryParseString "</map>" Xml.Decode.nodeTail)
                    (Ok ( "map", "" ))
        , test "Xml.Decode.attr" <|
            \() ->
                Expect.equal
                    (tryParse (String.toList "ID=\"123\"") Xml.Decode.attr)
                    (Ok
                        ( { name = "ID"
                          , value = "123"
                          }
                        , []
                        )
                    )
        , test "Xml.Decode.nodeHead" <|
            \() ->
                Expect.equal
                    (tryParseString "<map ID=\"123\">" Xml.Decode.nodeHead)
                    (Ok ( ( "map", [ { name = "ID", value = "123" } ] ), "" ))
        , test "Xml.Decode.node" <|
            \() ->
                Expect.equal
                    (tryParseString "<map id=\"123\"></map>" Xml.Decode.node)
                    (Ok ( Xml.Decode.Node "map" [ { name = "id", value = "123" } ] [], "" ))
        , test "Xml.Decode.node with spaces" <|
            \() ->
                Expect.equal
                    (tryParseString "<map id=\"123\"> </map>" Xml.Decode.node)
                    (Ok ( Xml.Decode.Node "map" [ { name = "id", value = "123" } ] [], "" ))
        , test "Xml.Decode.node with spaces" <|
            \() ->
                Expect.equal
                    (tryParseString "<map id=\"123\"><node></node></map>" Xml.Decode.node)
                    (Ok
                        ( Xml.Decode.Node
                            "map"
                            [ { name = "id", value = "123" } ]
                            [ Xml.Decode.Node "node" [] [] ]
                        , ""
                        )
                    )
        , test "Xml.Decode.node <- nested node" <|
            \() ->
                Expect.equal
                    (tryParseString "<map><node></node></map>" Xml.Decode.node)
                    (Ok
                        ( Xml.Decode.Node "map"
                            []
                            [ Xml.Decode.Node "node" [] []
                            ]
                        , ""
                        )
                    )
        , test "Xml.Decode.node <- nested node with attr" <|
            \() ->
                Expect.equal
                    (tryParseString "<map version=\"0.7.1\"><node id=\"1\"></node></map>" Xml.Decode.node)
                    (Ok
                        ( Xml.Decode.Node "map"
                            [ { name = "version", value = "0.7.1" } ]
                            [ Xml.Decode.Node "node"
                                [ { name = "id", value = "1" }
                                ]
                                []
                            ]
                        , ""
                        )
                    )
        , test "Xml.Decode.node <- nested node with multiple attrs" <|
            \() ->
                Expect.equal
                    (tryParseString "<map version=\"0.7.1\"><node id=\"1\" text=\"content\"></node></map>" Xml.Decode.node)
                    (Ok
                        ( Xml.Decode.Node "map"
                            [ { name = "version", value = "0.7.1" } ]
                            [ Xml.Decode.Node "node"
                                [ { name = "id", value = "1" }
                                , { name = "text", value = "content" }
                                ]
                                []
                            ]
                        , ""
                        )
                    )
        ]


realWorld =
    describe "Xml.Decode (Real-World Example)"
        [ test "empty map" <|
            \() ->
                let
                    simpleRawString =
                        "<map version=\"0.7.1\"></map>"
                in
                    Expect.equal
                        (tryParseString simpleRawString Xml.Decode.node)
                        (Ok ( Xml.Decode.Node "map" [ { name = "version", value = "0.7.1" } ] [], "" ))
        , test "single node" <|
            \() ->
                let
                    simpleRawString =
                        "<map version=\"0.7.1\"><node ID=\"1\" TEXT=\"test\"></node></map>"
                in
                    Expect.equal
                        (tryParseString simpleRawString Xml.Decode.node)
                        (Ok
                            ( Xml.Decode.Node "map"
                                [ { name = "version", value = "0.7.1" } ]
                                [ Xml.Decode.Node "node"
                                    [ { name = "ID", value = "1" }
                                    , { name = "TEXT", value = "test" }
                                    ]
                                    []
                                ]
                            , ""
                            )
                        )
        , test "simple map" <|
            \() ->
                let
                    simpleRawString =
                        "<map version=\"0.7.1\"><node ID=\"1\" TEXT=\"test\"><node ID=\"3\" TEXT=\"part&#x20;one\"><node ID=\"4\" TEXT=\"detail&#x20;1\"></node><node ID=\"5\" TEXT=\"detail&#x20;2\"></node></node><node ID=\"6\" TEXT=\"part&#x20;two\"></node></node></map>"
                in
                    Expect.true "failed to parse a simple map."
                        ((tryParseString simpleRawString Xml.Decode.node)
                            |> (\res ->
                                    case res of
                                        Ok ( _, "" ) ->
                                            True

                                        _ ->
                                            False
                               )
                        )
        ]
