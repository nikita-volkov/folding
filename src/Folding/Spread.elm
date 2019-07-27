module Folding.Spread exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Folding.Fold as Fold exposing (Fold)

{-| A projection on data, which only knows how to execute a strict left-fold.
You may also know it as Church-Encoded List.

It provides an interface similar to that of most collection datatypes,
with a key difference, that all its transforming and constructing
operations have complexity of `O(1)`.
Hence if you need to do series of `map`, `filter`, `concat`, `append` operations
on one or more collections, always prefer first converting the
datastructures to `Spread`, then performing the required operations on it,
and then materializing it into any final datastructure that you need.

In its way this abstraction achieves goals similar to the ones of Clojure's transducers,
in case you've heard of them.
It just approaches the problem from a different perspective.

The performance characteristics are achieved because,
all transforming and constructing operations don't actually materialize
into any datastructure and produce a folding function instead.
In other words, they are lazy.

## Intuition

An intuition for this abstraction can be derived from lists.

Let's consider the `List.foldl` function:

    foldl :: (a -> b -> b) -> b -> List a -> b

If we rearrange its parameters a bit, we can get

    foldl :: List a -> (a -> b -> b) -> b -> b

Which in Elm is essentially the same as

    foldl :: List a -> ((a -> b -> b) -> b -> b)

If we isolate the grouped part into an abstraction of its own

    type alias Spread b a = (a -> b -> b) -> b -> b

Then we can get to this simple morphism:

    list :: List a -> Spread b a
    list x = \ step init -> List.foldl step init x

We can do the same with any other datastructure that can be folded over, say `String`:

    string : String -> Spread b Char
    string x = \ step init -> String.foldl step init x

And then we can use those both to concatenate with just an `O(1)` cost:

    abcdef :: Spread b Char
    abcdef = prepend (list ['a', 'b', 'c']) (string "def")

Please notice that up until this moment no actual data materialization has happened and
hence no traversals have appeared.
All that we've done is just composed a function,
which only specifies which parts of data structures to traverse to perform a left-fold.
Only at the moment where we execute it will we actually traverse the source data
and we'll do that once only.
E.g., using the `toString` function:

    abcdef :: String
    abcdef = toString abcdef

## Concerning the `state` type parameter

Long story short, it really doesn't matter.
Just specify whatever the compiler expects you to specify for it.
To getter a deeper insight on the matter read along.

Let's get back to the rearranged definition of `foldl`:

    foldl :: List a -> ((a -> b -> b) -> b -> b)

If only Elm supported the `forall` quantification like Haskell,
the above signature would actually be the same as the following:

    foldl :: List a -> (forall b. (a -> b -> b) -> b -> b)

Which would then mean, that we'd be able to drop the redundant `b` parameter
from the definition of `Spread` altogether:

    type alias Spread a = forall b. (a -> b -> b) -> b -> b

In fact this is actually [the way it is defined in the "deferred-folds" Haskell library](https://hackage.haskell.org/package/deferred-folds-0.9.10.1/docs/DeferredFolds-Spread.html#t:Spread),
which this one draws the inspiration from.
-}
type alias Spread state element = (element -> state -> state) -> state -> state

-- * Constructors

empty : Spread s element
empty = always identity

list : List element -> Spread s element
list x step init = List.foldl step init x

set : Set element -> Spread s element
set x step init = Set.foldl step init x

array : Array element -> Spread s element
array x step init = Array.foldl step init x

string : String -> Spread s Char
string x step init = String.foldl step init x

dict : Dict key value -> Spread s (key, value)
dict x step init = Dict.foldl (\ k v -> step (k, v)) init x

range : Int -> Int -> Spread s Int
range first last step =
  let
    loop current state =
      if current <= last
        then loop (current + 1) (step current state)
        else state
    in loop first

-- * Transformations

cons : element -> Spread s element -> Spread s element
cons element spread step init = spread step (step element init)

snoc : element -> Spread s element -> Spread s element
snoc element spread step init = step element (spread step init)

append : Spread s element -> Spread s element -> Spread s element
append left right step init = left step (right step init)

prepend : Spread s element -> Spread s element -> Spread s element
prepend left right step init = right step (left step init)

map : (a -> b) -> Spread s a -> Spread s b
map fn spread step = spread (\ element -> step (fn element))

concat : List (Spread s element) -> Spread s element
concat x step init = List.foldl (\ innerSpread -> innerSpread step) init x

concatMap : (a -> List b) -> Spread s a -> Spread s b
concatMap b a step init = a (\ element state -> List.foldl step state (b element)) init

join : Spread s (Spread s element) -> Spread s element
join spread step init = spread (\ innerSpread -> innerSpread step) init

joinMap : (a -> Spread s b) -> Spread s a -> Spread s b
joinMap b a step init = a (\ element -> b element step) init

filter : (element -> Bool) -> Spread s element -> Spread s element
filter fn spread step init =
  let
    newStep a state = if fn a
      then step a state
      else state
    in spread newStep init

filterMap : (a -> Maybe b) -> Spread s a -> Spread s b
filterMap fn spread step init =
  let
    newStep a state = case fn a of
      Just b -> step b state
      Nothing -> state
    in spread newStep init

index : Spread (Int, s) element -> Spread s (Int, element)
index spread step init =
  let
    newStep a (i, state) = (i + 1, step (i, a) state)
    in Tuple.second (spread newStep (0, init))

unique : Spread (Set comparable, s) comparable -> Spread s comparable
unique spread step =
  let
    newStep element (known, state) = if Set.member element known
      then (known, state)
      else (Set.insert element known, step element state)
    in \ init -> spread newStep (Set.empty, init) |> Tuple.second

-- * Materialization

fold : Fold s element result -> Spread s element -> result
fold f spread = f.finish (spread f.step f.init)

toList : Spread (List element) element -> List element
toList = fold Fold.list

toReverseList : Spread (List element) element -> List element
toReverseList = fold Fold.reverseList

toString : Spread (List Char) Char -> String
toString = fold Fold.string

toSet : Spread (Set comparable) comparable -> Set comparable
toSet = fold Fold.set

toArray : Spread (List element) element -> Array element
toArray = fold Fold.array
