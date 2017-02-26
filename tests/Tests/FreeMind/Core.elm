module Tests.FreeMind.Core exposing (all)

import Expect
import FreeMind.Decode
import Test exposing (Test, describe, test)


all : Test
all =
    describe "MindMap Core Test Suite"
        [ test "Simple Map" <|
            \() ->
                let
                    simpleRawString =
                        "<map version=\"0.7.1\"><node ID=\"1\" TEXT=\"test\"><node ID=\"3\" TEXT=\"part&#x20;one\"><node ID=\"4\" TEXT=\"detail&#x20;1\"></node><node ID=\"5\" TEXT=\"detail&#x20;2\"></node></node><node ID=\"6\" TEXT=\"part&#x20;two\"></node></node></map>"
                in
                    Expect.equal
                        (FreeMind.Decode.decodeMap simpleRawString)
                        (Just
                            ([ FreeMind.Decode.Node "1"
                                "test"
                                ([ FreeMind.Decode.Node "3"
                                    "part one"
                                    ([ FreeMind.Decode.Node "4" "detail 1" []
                                     , FreeMind.Decode.Node "5" "detail 2" []
                                     ]
                                    )
                                 , FreeMind.Decode.Node "6" "part two" []
                                 ]
                                )
                             ]
                            )
                        )
        ]
