module EveryBiDictTests exposing (..)

import Set
import Test exposing (..)
import Expect
import Fuzz
import EveryBiDict exposing (EveryBiDict)
import EveryDict exposing (EveryDict)


uniqueIntPairListFuzzer : Fuzz.Fuzzer (List ( Int, Int ))
uniqueIntPairListFuzzer =
    Fuzz.list (Fuzz.tuple ( Fuzz.intRange 0 100, Fuzz.intRange 0 100 ))
        |> Fuzz.map (Set.fromList >> Set.toList)


bidictFuzzer : Fuzz.Fuzzer (EveryBiDict Int Int)
bidictFuzzer =
    uniqueIntPairListFuzzer
        |> Fuzz.map EveryBiDict.fromList


bidictIntTupleFuzzer : Fuzz.Fuzzer ( EveryBiDict Int Int, Int, Int )
bidictIntTupleFuzzer =
    Fuzz.tuple3 ( bidictFuzzer, Fuzz.int, Fuzz.int )


all : Test
all =
    describe "EveryBiDict Tests"
        [ describe "insert Tests"
            [ fuzz bidictIntTupleFuzzer "insert m n >> getByFirst m >> member n" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.getByFirst m
                        |> EveryDict.member n
                        |> Expect.true "Expected the BiDict to contain ( m, n )"
            , fuzz bidictIntTupleFuzzer "insert m n >> getBySecond n >> member m" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.getBySecond n
                        |> EveryDict.member m
                        |> Expect.true "Expected the BiDict to contain ( m, n )"
            ]
        , describe "More insert Tests"
            [ fuzz bidictIntTupleFuzzer "insert m n >> getByFirst m >> member n" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.insert m (0 - n)
                        |> EveryBiDict.insert (0 - m) n
                        |> EveryBiDict.getByFirst m
                        |> EveryDict.member n
                        |> Expect.true "Expected the BiDict to contain ( m, n )"
            , fuzz bidictIntTupleFuzzer "insert m n >> getBySecond n >> member m" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.insert m (0 - n)
                        |> EveryBiDict.insert (0 - m) n
                        |> EveryBiDict.getBySecond n
                        |> EveryDict.member m
                        |> Expect.true "Expected the BiDict to contain ( m, n )"
            ]
        , describe "removeOne Tests"
            [ fuzz bidictIntTupleFuzzer "insert m n >> removeOne m n >> getByFirst m >> (not << member n)" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.removeOne m n
                        |> EveryBiDict.getByFirst m
                        |> EveryDict.member n
                        |> Expect.false "Expected the BiDict not to contain ( m, n )"
            , fuzz bidictIntTupleFuzzer "insert m n >> removeOne m n >> getBySecond n >> (not << member m)" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.removeOne m n
                        |> EveryBiDict.getBySecond n
                        |> EveryDict.member m
                        |> Expect.false "Expected the BiDict not to contain ( m, n )"
            ]
        , describe "removeByFirst and removeBySecond Tests"
            [ fuzz bidictIntTupleFuzzer "insert m n >> removeByFirst m >> getByFirst m >> (== empty)" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.removeByFirst m
                        |> EveryBiDict.getByFirst m
                        |> Expect.equal EveryDict.empty
            , fuzz bidictIntTupleFuzzer "insert m n >> removeBySecond n >> getBySecond n >> (== empty)" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.removeBySecond n
                        |> EveryBiDict.getBySecond n
                        |> Expect.equal EveryDict.empty
            , fuzz bidictIntTupleFuzzer "insert m n >> removeByFirst m >> getBySecond n >> (not << member m)" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.removeByFirst m
                        |> EveryBiDict.getBySecond n
                        |> EveryDict.member m
                        |> Expect.false "Expected the BiDict not to contain ( m, n )"
            , fuzz bidictIntTupleFuzzer "insert m n >> removeBySecond n >> getByFirst m >> (not << member n)" <|
                \( bd, m, n ) ->
                    bd
                        |> EveryBiDict.insert m n
                        |> EveryBiDict.removeBySecond n
                        |> EveryBiDict.getByFirst m
                        |> EveryDict.member n
                        |> Expect.false "Expected the BiDict not to contain ( m, n )"
            ]
        , describe "toList and fromList Tests"
            [ fuzz bidictFuzzer "toList >> fromList === identity" <|
                \bd ->
                    bd
                        |> EveryBiDict.toList
                        |> EveryBiDict.fromList
                        |> Expect.equal bd
            , fuzz uniqueIntPairListFuzzer "fromList >> toList === identity" <|
                \ipl ->
                    ipl
                        |> EveryBiDict.fromList
                        |> EveryBiDict.toList
                        |> List.sort
                        |> Expect.equal (ipl |> List.sort)
            ]
        ]
