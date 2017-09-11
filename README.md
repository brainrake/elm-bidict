# Many-to-Many relation using a pair of Dicts

[![Build Status](https://travis-ci.org/brainrape/elm-bidict.svg?branch=master)](https://travis-ci.org/brainrape/elm-bidict)

`BiDict m n` stores related value. Each value of one type has an associated set of the other type.

This is implemented using a pair of `Dict`s of values of one type to sets of values of the other type. One `Dict` in each direction.

There is also an `EveryBiDict` where you can store non-comparable values. It is based on `EveryDict` from [eeue56/elm-all-dict](http://package.elm-lang.org/packages/eeue56/elm-all-dict/latest), comparing values using a custom `toString`. You should make sure your values work correctly with it.
