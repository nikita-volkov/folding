module Folding.Spread exposing (..)

import Folding.Types exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Folding.Fold as Fold exposing (Fold)


-- * Constructors

empty : Spread s element
empty = always identity

singleton : element -> Spread s element
singleton element step init = step element init

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
