module EverySetDict exposing (EverySetDict, insert, get, removeOne, removeMany)

{-|
A Many-to-One relationship using an EveryDict of Sets.
This is a specialization of EveryDict so all EveryDict functions are available.

# Definition
@docs EverySetDict

# Operations
@docs insert, get, removeOne, removeMany
-}

import EveryDict exposing (EveryDict)


{-| An EveryDict mapping keys to sets of values
-}
type alias EverySetDict m n =
    EveryDict m (EveryDict n ())


{-| Given a key and value, Insert a value into the set of values associated with the key
-}
insert : m -> n -> (EverySetDict m n -> EverySetDict m n)
insert m n =
    EveryDict.update m
        (\m_ns ->
            case m_ns of
                Just ns ->
                    Just (ns |> EveryDict.insert n ())

                Nothing ->
                    Just (EveryDict.singleton n ())
        )


{-| Return the set of values associated with a key. It may be empty.
-}
get : m -> EverySetDict m n -> EveryDict n ()
get m setdict =
    case EveryDict.get m setdict of
        Just v ->
            v

        Nothing ->
            EveryDict.empty


{-| Given a key and value, remove the value from the set of values associated with the keys
-}
removeOne : m -> n -> (EverySetDict m n -> EverySetDict m n)
removeOne m n =
    EveryDict.update m (Maybe.map (EveryDict.remove n))


{-| Remove all values associated with a key
-}
removeMany : m -> (EverySetDict m n -> EverySetDict m n)
removeMany =
    EveryDict.remove
