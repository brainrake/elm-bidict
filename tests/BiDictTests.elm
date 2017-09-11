module BiDictTests exposing (..)

import Set
import Test exposing (..)
import Expect
import Fuzz
import Dict
import BiDict exposing (BiDict)


uniqueIntPairListFuzzer : Fuzz.Fuzzer (List ( Int, Int ))
uniqueIntPairListFuzzer =
    Fuzz.list (Fuzz.tuple ( Fuzz.intRange 0 100, Fuzz.intRange 0 100 ))
        |> Fuzz.map (Set.fromList >> Set.toList)


bidictFuzzer : Fuzz.Fuzzer (BiDict Int Int)
bidictFuzzer =
    uniqueIntPairListFuzzer
        |> Fuzz.map BiDict.fromList


bidictIntTupleFuzzer : Fuzz.Fuzzer ( BiDict Int Int, Int, Int )
bidictIntTupleFuzzer =
    Fuzz.tuple3 ( bidictFuzzer, Fuzz.int, Fuzz.int )


all : Test
all =
    describe "BiDict Tests"
        [ describe "insert Tests"
            [ fuzz bidictIntTupleFuzzer "insert m n >> getByFirst m >> member n" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.getByFirst m
                        |> Dict.member n
                        |> Expect.true "Expected the BiDict to contain ( m, n )"
            , fuzz bidictIntTupleFuzzer "insert m n >> getBySecond n >> member m" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.getBySecond n
                        |> Dict.member m
                        |> Expect.true "Expected the BiDict to contain ( m, n )"
            ]
        , describe "More insert Tests"
            [ fuzz bidictIntTupleFuzzer "insert m n >> getByFirst m >> member n" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.insert m (0 - n)
                        |> BiDict.insert (0 - m) n
                        |> BiDict.getByFirst m
                        |> Dict.member n
                        |> Expect.true "Expected the BiDict to contain ( m, n )"
            , fuzz bidictIntTupleFuzzer "insert m n >> getBySecond n >> member m" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.insert m (0 - n)
                        |> BiDict.insert (0 - m) n
                        |> BiDict.getBySecond n
                        |> Dict.member m
                        |> Expect.true "Expected the BiDict to contain ( m, n )"
            ]
        , describe "removeOne Tests"
            [ fuzz bidictIntTupleFuzzer "insert m n >> removeOne m n >> getByFirst m >> (not << member n)" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.removeOne m n
                        |> BiDict.getByFirst m
                        |> Dict.member n
                        |> Expect.false "Expected the BiDict not to contain ( m, n )"
            , fuzz bidictIntTupleFuzzer "insert m n >> removeOne m n >> getBySecond n >> (not << member m)" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.removeOne m n
                        |> BiDict.getBySecond n
                        |> Dict.member m
                        |> Expect.false "Expected the BiDict not to contain ( m, n )"
            ]
        , describe "removeByFirst and removeBySecond Tests"
            [ fuzz bidictIntTupleFuzzer "insert m n >> removeByFirst m >> getByFirst m >> (== empty)" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.removeByFirst m
                        |> BiDict.getByFirst m
                        |> Expect.equal Dict.empty
            , fuzz bidictIntTupleFuzzer "insert m n >> removeBySecond n >> getBySecond n >> (== empty)" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.removeBySecond n
                        |> BiDict.getBySecond n
                        |> Expect.equal Dict.empty
            , fuzz bidictIntTupleFuzzer "insert m n >> removeByFirst m >> getBySecond n >> (not << member m)" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.removeByFirst m
                        |> BiDict.getBySecond n
                        |> Dict.member m
                        |> Expect.false "Expected the BiDict not to contain ( m, n )"
            , fuzz bidictIntTupleFuzzer "insert m n >> removeBySecond n >> getByFirst m >> (not << member n)" <|
                \( bd, m, n ) ->
                    bd
                        |> BiDict.insert m n
                        |> BiDict.removeBySecond n
                        |> BiDict.getByFirst m
                        |> Dict.member n
                        |> Expect.false "Expected the BiDict not to contain ( m, n )"
            ]
        , describe "toList and fromList Tests"
            [ fuzz bidictFuzzer "toList >> fromList === identity" <|
                \bd ->
                    bd
                        |> BiDict.toList
                        |> BiDict.fromList
                        |> Expect.equal bd
            , fuzz uniqueIntPairListFuzzer "fromList >> toList === identity" <|
                \ipl ->
                    ipl
                        |> BiDict.fromList
                        |> BiDict.toList
                        |> List.sort
                        |> Expect.equal (ipl |> List.sort)
            ]
        ]
