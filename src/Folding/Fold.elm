module Folding.Fold exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Either exposing (Either(..))
import Set exposing (Set)

type alias Fold state element result =
  {
    init : state,
    step : element -> state -> state,
    finish : state -> result
  }

-- * Constructors

simple : (a -> b -> b) -> b -> Fold b a b
simple step init =
  {
    init = init,
    step = step,
    finish = identity
  }

finalized : (a -> b -> b) -> b -> (b -> c) -> Fold b a c
finalized step init finish =
  {
    init = init,
    step = step,
    finish = finish
  }

length : Fold Int a Int
length = simple (\ _ x -> x + 1) 0

maxWithInit : comparable -> Fold comparable comparable comparable
maxWithInit init =
  {
    init = init,
    step = max,
    finish = identity
  }

reverseList : Fold (List a) a (List a)
reverseList = simple (::) []

list : Fold (List a) a (List a)
list = finalized (::) [] List.reverse

set : Fold (Set comparable) comparable (Set comparable)
set = simple Set.insert Set.empty

array : Fold (List a) a (Array a)
array = mapOutput Array.fromList list

string : Fold (List Char) Char String
string = finalized (::) [] (List.reverse >> String.fromList)

either : Fold x1 a1 b1 -> Fold x2 a2 b2 -> Fold (x1, x2) (Either a1 a2) (b1, b2)
either left right =
  {
    init = (left.init, right.init),
    step = \ x (leftState, rightState) -> case x of
      Left input -> (left.step input leftState, rightState)
      Right input -> (leftState, right.step input rightState),
    finish = \ (leftInput, rightInput) -> (left.finish leftInput, right.finish rightInput)
  }

-- * Transformations

mapOutput : (a -> b) -> Fold state input a -> Fold state input b
mapOutput fn fold = { init = fold.init, step = fold.step, finish = fold.finish >> fn }

mapInput : (a -> b) -> Fold state b output -> Fold state a output
mapInput fn fold =
  {
    init = fold.init,
    step = \ a state -> fold.step (fn a) state,
    finish = fold.finish
  }

filterMapInput : (a -> Maybe b) -> Fold state b output -> Fold state a output
filterMapInput fn fold =
  {
    init = fold.init,
    step = \ a state -> case fn a of
      Just b -> fold.step b state
      Nothing -> state,
    finish = fold.finish
  }
