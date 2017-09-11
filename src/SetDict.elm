module SetDict exposing (SetDict, insert, get, removeOne, removeMany)

{-|
A Many-to-One relationship using a Dict of Sets.
This is a specialization of Dict so all Dict functions are available.

# Definition
@docs SetDict

# Operations
@docs insert, get, removeOne, removeMany
-}

import Dict exposing (Dict)


{-| A Dict mapping keys to sets of values
-}
type alias SetDict comparable1 comparable2 =
    Dict comparable1 (Dict comparable2 ())


{-| Given a key and value, Insert a value into the set of values associated with the key
-}
insert : comparable1 -> comparable2 -> (SetDict comparable1 comparable2 -> SetDict comparable1 comparable2)
insert m n =
    Dict.update m
        (\m_ns ->
            case m_ns of
                Just ns ->
                    Just (ns |> Dict.insert n ())

                Nothing ->
                    Just (Dict.singleton n ())
        )


{-| Return the set of values associated with a key. It may be empty.
-}
get : comparable1 -> SetDict comparable1 comparable2 -> Dict comparable2 ()
get m setdict =
    case Dict.get m setdict of
        Just v ->
            v

        Nothing ->
            Dict.empty


{-| Given a key and value, remove the value from the set of values associated with the keys
-}
removeOne : comparable1 -> comparable2 -> (SetDict comparable1 comparable2 -> SetDict comparable1 comparable2)
removeOne m n =
    Dict.update m (Maybe.map (Dict.remove n))


{-| Remove all values associated with a key
-}
removeMany : comparable1 -> (SetDict comparable1 comparable2 -> SetDict comparable1 comparable2)
removeMany =
    Dict.remove
