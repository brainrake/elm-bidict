module Tests exposing (..)

import Test exposing (..)
import SetDictTests
import EverySetDictTests
import BiDictTests
import EveryBiDictTests


all : Test
all =
    describe "Test Suite"
        [ SetDictTests.all
        , EverySetDictTests.all
        , BiDictTests.all
        , EveryBiDictTests.all
        ]
