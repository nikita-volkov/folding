module Folding.Reduction exposing (..)

import Folding.Types exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import Either exposing (Either(..))
import Set exposing (Set)


-- * Execution
-------------------------

reduceList : Reduction a b -> List a -> b
reduceList reduction list = case reduction of
  Ongoing o nextReduction -> case list of
    i :: nextList -> reduceList (nextReduction i) nextList
    _ -> o
  Terminated o -> o

reduceSpread : Reduction a b -> Spread (Reduction a b) a -> b
reduceSpread initialReduction spread =
  let
    finalReduction =
      spread
        (\ input reduction -> case reduction of
          Ongoing _ nextReduction -> nextReduction input
          _ -> reduction
        )
        initialReduction
    in case finalReduction of
      Ongoing output _ -> output
      Terminated output -> output

reduceFolding : Folding (Reduction a b) a c -> Reduction a b -> c -> b
reduceFolding folding reduction foldable =
  reduceSpread reduction (\ step state -> folding step state foldable)

toFold : Reduction a b -> Fold (Reduction a b) a b
toFold initialReduction =
  {
    init = initialReduction,
    step = \ input reduction -> case reduction of
      Ongoing _ nextReduction -> nextReduction input
      _ -> reduction,
    finish = \ reduction -> case reduction of
      Ongoing o _ -> o
      Terminated o -> o
  }


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
