module EveryBiDict exposing (EveryBiDict, empty, insert, getByFirst, getBySecond, removeOne, removeByFirst, removeBySecond, fromList, toList)

{-| Many to Many Relationship using a pair of Dicts

# Definition
@docs EveryBiDict

# Build
@docs empty, insert

# Query
@docs getByFirst, getBySecond

# Remove
@docs removeOne, removeByFirst, removeBySecond

# Lists
@docs fromList, toList
-}

import EveryDict exposing (EveryDict)
import EverySetDict exposing (EverySetDict)


{-| A pair of EveryDicts, mapping a set of first values to a set of second values, and back. The operations keep the Dicts in sync.
-}
type alias EveryBiDict m n =
    ( EverySetDict m n, EverySetDict n m )


{-| An empty BiDict
-}
empty : EveryBiDict m n
empty =
    ( EveryDict.empty, EveryDict.empty )


{-| Insert a relationship between a first and a second value
-}
insert : m -> n -> EveryBiDict m n -> EveryBiDict m n
insert m n ( d1, d2 ) =
    ( d1 |> EverySetDict.insert m n
    , d2 |> EverySetDict.insert n m
    )


{-| Get the set of second values associated with a given first value
-}
getByFirst : m -> EveryBiDict m n -> EveryDict n ()
getByFirst m ( d1, _ ) =
    EveryDict.get m d1 |> Maybe.withDefault EveryDict.empty


{-| Get the set first values associated with a given second value
-}
getBySecond : n -> EveryBiDict m n -> EveryDict m ()
getBySecond n ( _, d2 ) =
    EveryDict.get n d2 |> Maybe.withDefault EveryDict.empty


{-| Remove the given relationship
-}
removeOne : m -> n -> EveryBiDict m n -> EveryBiDict m n
removeOne m n ( d1, d2 ) =
    ( d1 |> EverySetDict.removeOne m n
    , d2 |> EverySetDict.removeOne n m
    )


{-| Remove all relationships for the given first value
-}
removeByFirst : m -> EveryBiDict m n -> EveryBiDict m n
removeByFirst m ( d1, d2 ) =
    ( d1 |> EverySetDict.removeMany m
    , d1
        |> EveryDict.get m
        |> Maybe.map (EveryDict.keys >> List.foldr EverySetDict.removeMany d2)
        |> Maybe.withDefault d2
    )


{-| Remove all relationships for the given second value
-}
removeBySecond : n -> EveryBiDict m n -> EveryBiDict m n
removeBySecond n ( d1, d2 ) =
    ( d2
        |> EveryDict.get n
        |> Maybe.map (EveryDict.keys >> List.foldr EverySetDict.removeMany d1)
        |> Maybe.withDefault d1
    , d2 |> EverySetDict.removeMany n
    )


{-| Return the list of pairs of first and second values
-}
toList : EveryBiDict m n -> List ( m, n )
toList ( d1, d2 ) =
    d1
        |> EveryDict.toList
        |> List.concatMap
            (\( m, ns ) ->
                ns
                    |> EveryDict.keys
                    |> List.map (\n -> ( m, n ))
            )


{-| Build a BiDict from a list of pairs
-}
fromList : List ( m, n ) -> EveryBiDict m n
fromList =
    List.foldr (uncurry insert) empty
