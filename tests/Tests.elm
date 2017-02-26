module Tests exposing (..)

import FreeMind.Decode
import LangUtils
import NonEmptyList
import Test exposing (..)
import Expect
import Fuzz
import String
import Tests.FreeMind.Core
import Tests.FreeMind.Lib
import Tests.FreeMind.Tests
import Xml.Decode exposing (Node)


all : Test
all =
    Tests.FreeMind.Tests.all
