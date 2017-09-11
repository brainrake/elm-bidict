module BiDict exposing (BiDict, empty, insert, getByFirst, getBySecond, removeOne, removeByFirst, removeBySecond, fromList, toList)

{-| Many to Many Relationship using a pair of Dicts

# Definition
@docs BiDict

# Build
@docs empty, insert

# Query
@docs getByFirst, getBySecond

# Remove
@docs removeOne, removeByFirst, removeBySecond

# Lists
@docs fromList, toList
-}

import Dict exposing (Dict)
import SetDict exposing (SetDict)


{-| A pair of Dicts, mapping a set of first values to a set of second values, and back. The operations keep the Dicts in sync.
-}
type alias BiDict m n =
    ( SetDict m n, SetDict n m )


{-| An empty BiDict
-}
empty : BiDict m n
empty =
    ( Dict.empty, Dict.empty )


{-| Insert a relationship between a first and a second value
-}
insert : comparable1 -> comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
insert m n ( d1, d2 ) =
    ( d1 |> SetDict.insert m n
    , d2 |> SetDict.insert n m
    )


{-| Get the set of second values associated with a given first value
-}
getByFirst : comparable1 -> BiDict comparable1 comparable2 -> Dict comparable2 ()
getByFirst m ( d1, _ ) =
    Dict.get m d1 |> Maybe.withDefault Dict.empty


{-| Get the set first values associated with a given second value
-}
getBySecond : comparable2 -> BiDict comparable1 comparable2 -> Dict comparable1 ()
getBySecond n ( _, d2 ) =
    Dict.get n d2 |> Maybe.withDefault Dict.empty


{-| Remove the given relationship
-}
removeOne : comparable1 -> comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
removeOne m n ( d1, d2 ) =
    ( d1 |> SetDict.removeOne m n
    , d2 |> SetDict.removeOne n m
    )


{-| Remove all relationships for the given first value
-}
removeByFirst : comparable1 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
removeByFirst m ( d1, d2 ) =
    ( d1 |> SetDict.removeMany m
    , d1
        |> Dict.get m
        |> Maybe.map (Dict.keys >> List.foldr SetDict.removeMany d2)
        |> Maybe.withDefault d2
    )


{-| Remove all relationships for the given second value
-}
removeBySecond : comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
removeBySecond n ( d1, d2 ) =
    ( d2
        |> Dict.get n
        |> Maybe.map (Dict.keys >> List.foldr SetDict.removeMany d1)
        |> Maybe.withDefault d1
    , d2 |> SetDict.removeMany n
    )


{-| Return the list of pairs of first and second values
-}
toList : BiDict comparable1 comparable2 -> List ( comparable1, comparable2 )
toList ( d1, d2 ) =
    d1
        |> Dict.toList
        |> List.concatMap
            (\( m, ns ) ->
                ns
                    |> Dict.keys
                    |> List.map (\n -> ( m, n ))
            )


{-| Build a BiDict from a list of pairs
-}
fromList : List ( comparable1, comparable2 ) -> BiDict comparable1 comparable2
fromList =
    List.foldr (uncurry insert) empty
