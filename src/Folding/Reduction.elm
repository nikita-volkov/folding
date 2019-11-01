module Folding.Reduction exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Either exposing (Either(..))
import Set exposing (Set)


type Reduction i o =
  Ongoing o (i -> Reduction i o) |
  Terminated o


-- * Execution
-------------------------

reduceList : Reduction a b -> List a -> b
reduceList reduction list = case reduction of
  Ongoing o nextReduction -> case list of
    i :: nextList -> reduceList (nextReduction i) nextList
    _ -> o
  Terminated o -> o


-- * Construction
-------------------------

foldl : (a -> b -> b) -> b -> Reduction a b
foldl step =
  let
    fromState state = Ongoing state (\ a -> fromState (step a state))
    in fromState

count : Reduction a Int
count =
  let
    counted n = Ongoing n (\ _ -> counted (n + 1))
    in counted 0

buildReverseList : Reduction a (List a)
buildReverseList = foldl (::) []

buildList : Reduction a (List a)
buildList = mapOutput List.reverse buildReverseList


-- * Transformation
-------------------------

mapOutput : (o1 -> o2) -> Reduction i o1 -> Reduction i o2
mapOutput = Debug.todo ""

mapInput : (a -> b) -> Reduction b output -> Reduction a output
mapInput fn fold = Debug.todo ""
