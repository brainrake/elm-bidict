module EverySetDictTests exposing (..)

import Set
import Test exposing (..)
import Expect
import Fuzz
import EveryDict exposing (EveryDict)
import EverySetDict exposing (EverySetDict)


uniqueIntPairListFuzzer : Fuzz.Fuzzer (List ( Int, Int ))
uniqueIntPairListFuzzer =
    Fuzz.list (Fuzz.tuple ( Fuzz.intRange 0 100, Fuzz.intRange 0 100 ))
        |> Fuzz.map (Set.fromList >> Set.toList)


setdictFuzzer : Fuzz.Fuzzer (EverySetDict Int Int)
setdictFuzzer =
    uniqueIntPairListFuzzer
        |> Fuzz.map (List.foldr (uncurry EverySetDict.insert) EveryDict.empty)


setdictIntTupleFuzzer : Fuzz.Fuzzer ( EverySetDict Int Int, Int, Int )
setdictIntTupleFuzzer =
    Fuzz.tuple3 ( setdictFuzzer, Fuzz.int, Fuzz.int )


all : Test
all =
    describe "EverySetDict Tests"
        [ describe "insert Tests"
            [ fuzz setdictIntTupleFuzzer "insert m n >> getByFirst m >> member n" <|
                \( bd, m, n ) ->
                    bd
                        |> EverySetDict.insert m n
                        |> EverySetDict.get m
                        |> EveryDict.member n
                        |> Expect.true "Expected the EverySetDict to contain ( m, n )"
            ]
        , describe "More insert Tests"
            [ fuzz setdictIntTupleFuzzer "insert m n >> getByFirst m >> member n" <|
                \( bd, m, n ) ->
                    bd
                        |> EverySetDict.insert m n
                        |> EverySetDict.insert m (0 - n)
                        |> EverySetDict.insert (0 - m) n
                        |> EverySetDict.get m
                        |> EveryDict.member n
                        |> Expect.true "Expected the EverySetDict to contain ( m, n )"
            ]
        , describe "removeOne Tests"
            [ fuzz setdictIntTupleFuzzer "insert m n >> removeOne m n >> get m >> (not << member n)" <|
                \( bd, m, n ) ->
                    bd
                        |> EverySetDict.insert m n
                        |> EverySetDict.removeOne m n
                        |> EverySetDict.get m
                        |> EveryDict.member n
                        |> Expect.false "Expected the EverySetDict not to contain ( m, n )"
            ]
        , describe "removeMany Tests"
            [ fuzz setdictIntTupleFuzzer "insert m n >> removeMany m >> get m >> (== empty)" <|
                \( bd, m, n ) ->
                    bd
                        |> EverySetDict.insert m n
                        |> EverySetDict.removeMany m
                        |> EverySetDict.get m
                        |> Expect.equal EveryDict.empty
            ]
        ]
