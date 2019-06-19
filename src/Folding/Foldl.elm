module Folding.Foldl exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Either exposing (Either(..))
import Set exposing (Set)

type alias Foldl state element result =
  {
    init : state,
    step : element -> state -> state,
    finish : state -> result
  }

-- * Constructors

simple : (a -> b -> b) -> b -> Foldl b a b
simple step init =
  {
    init = init,
    step = step,
    finish = identity
  }

finalized : (a -> b -> b) -> b -> (b -> c) -> Foldl b a c
finalized step init finish =
  {
    init = init,
    step = step,
    finish = finish
  }

length : Foldl Int a Int
length = simple (\ _ x -> x + 1) 0

reverseList : Foldl (List a) a (List a)
reverseList = simple (::) []

list : Foldl (List a) a (List a)
list = finalized (::) [] List.reverse

set : Foldl (Set comparable) comparable (Set comparable)
set = simple Set.insert Set.empty

array : Foldl (List a) a (Array a)
array = mapOutput Array.fromList list

string : Foldl (List Char) Char String
string = finalized (::) [] (List.reverse >> String.fromList)

either : Foldl x1 a1 b1 -> Foldl x2 a2 b2 -> Foldl (x1, x2) (Either a1 a2) (b1, b2)
either left right =
  {
    init = (left.init, right.init),
    step = \ x (leftState, rightState) -> case x of
      Left input -> (left.step input leftState, rightState)
      Right input -> (leftState, right.step input rightState),
    finish = \ (leftInput, rightInput) -> (left.finish leftInput, right.finish rightInput)
  }

-- * Transformations

mapOutput : (a -> b) -> Foldl state input a -> Foldl state input b
mapOutput fn foldl = { init = foldl.init, step = foldl.step, finish = foldl.finish >> fn }

mapInput : (a -> b) -> Foldl state b output -> Foldl state a output
mapInput fn foldl =
  {
    init = foldl.init,
    step = \ a state -> foldl.step (fn a) state,
    finish = foldl.finish
  }

filterMapInput : (a -> Maybe b) -> Foldl state b output -> Foldl state a output
filterMapInput fn foldl =
  {
    init = foldl.init,
    step = \ a state -> case fn a of
      Just b -> foldl.step b state
      Nothing -> state,
    finish = foldl.finish
  }
