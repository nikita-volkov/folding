module Folding.Unfoldl exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Folding.Foldl as Foldl exposing (Foldl)

{-| A projection on data, which only knows how to execute a strict left-fold.
You may also know it as Church-Encoded List.

It provides an interface similar to that of most collection datatypes,
with a key difference, that all its transforming and constructing
operations have complexity of `O(1)`.
Hence if you need to do series of `map`, `filter`, `concat`, `append` operations
on one or more collections, always prefer first converting the
datastructures to `Unfoldl`, then performing the required operations on it,
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

    type alias Unfoldl b a = (a -> b -> b) -> b -> b

Then we can get to this simple morphism:

    list :: List a -> Unfoldl b a
    list x = \ step init -> List.foldl step init x

We can do the same with any other datastructure that can be folded over, say `String`:

    string : String -> Unfoldl b Char
    string x = \ step init -> String.foldl step init x

And then we can use those both to concatenate with just an `O(1)` cost:

    abcdef :: Unfoldl b Char
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
from the definition of `Unfoldl` altogether:

    type alias Unfoldl a = forall b. (a -> b -> b) -> b -> b

In fact this is actually [the way it is defined in the "deferred-folds" Haskell library](https://hackage.haskell.org/package/deferred-folds-0.9.10.1/docs/DeferredFolds-Unfoldl.html#t:Unfoldl),
which this one draws the inspiration from.
-}
type alias Unfoldl state element = (element -> state -> state) -> state -> state

-- * Constructors

empty : Unfoldl s element
empty = always identity

list : List element -> Unfoldl s element
list x step init = List.foldl step init x

set : Set element -> Unfoldl s element
set x step init = Set.foldl step init x

array : Array element -> Unfoldl s element
array x step init = Array.foldl step init x

string : String -> Unfoldl s Char
string x step init = String.foldl step init x

dict : Dict key value -> Unfoldl s (key, value)
dict x step init = Dict.foldl (\ k v -> step (k, v)) init x

range : Int -> Int -> Unfoldl s Int
range first last step =
  let
    loop current state =
      if current <= last
        then loop (current + 1) (step current state)
        else state
    in loop first

-- * Transformations

cons : element -> Unfoldl s element -> Unfoldl s element
cons element unfoldl step init = unfoldl step (step element init)

snoc : element -> Unfoldl s element -> Unfoldl s element
snoc element unfoldl step init = step element (unfoldl step init)

append : Unfoldl s element -> Unfoldl s element -> Unfoldl s element
append left right step init = left step (right step init)

prepend : Unfoldl s element -> Unfoldl s element -> Unfoldl s element
prepend left right step init = right step (left step init)

map : (a -> b) -> Unfoldl s a -> Unfoldl s b
map fn unfoldl step = unfoldl (\ element -> step (fn element))

concat : List (Unfoldl s element) -> Unfoldl s element
concat x step init = List.foldl (\ innerUnfoldl -> innerUnfoldl step) init x

concatMap : (a -> List b) -> Unfoldl s a -> Unfoldl s b
concatMap b a step init = a (\ element state -> List.foldl step state (b element)) init

join : Unfoldl s (Unfoldl s element) -> Unfoldl s element
join unfoldl step init = unfoldl (\ innerUnfoldl -> innerUnfoldl step) init

joinMap : (a -> Unfoldl s b) -> Unfoldl s a -> Unfoldl s b
joinMap b a step init = a (\ element -> b element step) init

filter : (element -> Bool) -> Unfoldl s element -> Unfoldl s element
filter fn unfoldl step init =
  let
    newStep a state = if fn a
      then step a state
      else state
    in unfoldl newStep init

filterMap : (a -> Maybe b) -> Unfoldl s a -> Unfoldl s b
filterMap fn unfoldl step init =
  let
    newStep a state = case fn a of
      Just b -> step b state
      Nothing -> state
    in unfoldl newStep init

index : Unfoldl (Int, s) element -> Unfoldl s (Int, element)
index unfoldl step init =
  let
    newStep a (i, state) = (i + 1, step (i, a) state)
    in Tuple.second (unfoldl newStep (0, init))

unique : Unfoldl (Set comparable, s) comparable -> Unfoldl s comparable
unique unfoldl step =
  let
    newStep element (known, state) = if Set.member element known
      then (known, state)
      else (Set.insert element known, step element state)
    in \ init -> unfoldl newStep (Set.empty, init) |> Tuple.second

-- * Materialization

foldl : Foldl s element result -> Unfoldl s element -> result
foldl f unfoldl = f.finish (unfoldl f.step f.init)

toList : Unfoldl (List element) element -> List element
toList = foldl Foldl.list

toReverseList : Unfoldl (List element) element -> List element
toReverseList = foldl Foldl.reverseList

toString : Unfoldl (List Char) Char -> String
toString = foldl Foldl.string

toSet : Unfoldl (Set comparable) comparable -> Set comparable
toSet = foldl Foldl.set

toArray : Unfoldl (List element) element -> Array element
toArray = foldl Foldl.array
