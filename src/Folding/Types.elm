module Folding.Types exposing (..)


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

type alias Fold state element result =
  {
    init : state,
    step : element -> state -> state,
    finish : state -> result
  }

type Reduction i o =
  Ongoing o (i -> Reduction i o) |
  Terminated o

type alias Folding state element container =
  (element -> state -> state) -> state -> container -> state
