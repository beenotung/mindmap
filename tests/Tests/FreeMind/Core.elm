module Tests.FreeMind.Core exposing (all)

import Expect
import FreeMind.Decode exposing (sampleRawString)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "MindMap Core Test Suite"
        [ test "Simple Map" <|
            \() ->
                Expect.equal
                    (FreeMind.Decode.decodeMap sampleRawString)
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
