module SetDictTests exposing (..)

import Set
import Test exposing (..)
import Expect
import Fuzz
import Dict
import SetDict exposing (SetDict)


uniqueIntPairListFuzzer : Fuzz.Fuzzer (List ( Int, Int ))
uniqueIntPairListFuzzer =
    Fuzz.list (Fuzz.tuple ( Fuzz.intRange 0 100, Fuzz.intRange 0 100 ))
        |> Fuzz.map (Set.fromList >> Set.toList)


setdictFuzzer : Fuzz.Fuzzer (SetDict Int Int)
setdictFuzzer =
    uniqueIntPairListFuzzer
        |> Fuzz.map (List.foldr (uncurry SetDict.insert) Dict.empty)


setdictIntTupleFuzzer : Fuzz.Fuzzer ( SetDict Int Int, Int, Int )
setdictIntTupleFuzzer =
    Fuzz.tuple3 ( setdictFuzzer, Fuzz.int, Fuzz.int )


all : Test
all =
    describe "SetDict Tests"
        [ describe "insert Tests"
            [ fuzz setdictIntTupleFuzzer "insert m n >> getByFirst m >> member n" <|
                \( bd, m, n ) ->
                    bd
                        |> SetDict.insert m n
                        |> SetDict.get m
                        |> Dict.member n
                        |> Expect.true "Expected the SetDict to contain ( m, n )"
            ]
        , describe "More insert Tests"
            [ fuzz setdictIntTupleFuzzer "insert m n >> getByFirst m >> member n" <|
                \( bd, m, n ) ->
                    bd
                        |> SetDict.insert m n
                        |> SetDict.insert m (0 - n)
                        |> SetDict.insert (0 - m) n
                        |> SetDict.get m
                        |> Dict.member n
                        |> Expect.true "Expected the SetDict to contain ( m, n )"
            ]
        , describe "removeOne Tests"
            [ fuzz setdictIntTupleFuzzer "insert m n >> removeOne m n >> get m >> (not << member n)" <|
                \( bd, m, n ) ->
                    bd
                        |> SetDict.insert m n
                        |> SetDict.removeOne m n
                        |> SetDict.get m
                        |> Dict.member n
                        |> Expect.false "Expected the SetDict not to contain ( m, n )"
            ]
        , describe "removeMany Tests"
            [ fuzz setdictIntTupleFuzzer "insert m n >> removeMany m >> get m >> (== empty)" <|
                \( bd, m, n ) ->
                    bd
                        |> SetDict.insert m n
                        |> SetDict.removeMany m
                        |> SetDict.get m
                        |> Expect.equal Dict.empty
            ]
        ]
