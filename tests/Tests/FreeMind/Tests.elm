module Tests.FreeMind.Tests exposing (all)

import Test exposing (Test, describe)
import Tests.FreeMind.Core
import Tests.FreeMind.Lib


all : Test
all =
    describe "MindMap Test Suite"
        [ Tests.FreeMind.Lib.all
        , Tests.FreeMind.Core.all
        ]
