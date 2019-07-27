module Spread exposing (..)

import Expect exposing (..)
import Fuzz exposing (..)
import Test exposing (..)
import Folding.Spread as Spread exposing (Spread)


listOps : Test
listOps =
  describe "Comparison to list ops"
    [
      fuzz (list string) "concat" <| \ stringList ->
        equal (String.concat stringList)
          (Spread.toString (Spread.concat (List.map Spread.string stringList)))
      ,
      fuzz (list int) "map" <| \ intList ->
        equal (List.map (\ x -> x + 1) intList)
          (Spread.toList (Spread.map (\ x -> x + 1) (Spread.list intList)))
      ,
      fuzz2 (list int) (list int) "prepend" <| \ a b ->
        equal (List.append a b)
          (Spread.toList (Spread.prepend (Spread.list a) (Spread.list b)))
      ,
      fuzz2 int (list int) "cons" <| \ a b ->
        equal (a :: b)
          (Spread.toList (Spread.cons a (Spread.list b)))
      ,
      fuzz2 int (list int) "snoc" <| \ a b ->
        equal (b ++ [a])
          (Spread.toList (Spread.snoc a (Spread.list b)))
      ,
      fuzz2 (intRange -100 100) (intRange -100 100) "range" <| \ a b ->
        equal (List.range a b)
          (Spread.toList (Spread.range a b))
    ]
