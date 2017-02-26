module Tests.FreeMind.Lib exposing (all)

import Expect
import Fuzz
import LangUtils
import NonEmptyList
import Parser exposing (any, element, satisfy, some, tryParse, tryParseString, int, float)
import Test exposing (Test, describe, fuzzWith, test)
import Tests.LangUtils.Tests
import Tests.Parser.Tests
import Tests.Xml.Tests
import Xml.Decode


all : Test
all =
    describe "FreeMind Library Test Suite"
        [ Tests.LangUtils.Tests.all
        , Tests.Parser.Tests.all
        , Tests.Xml.Tests.all
        ]
