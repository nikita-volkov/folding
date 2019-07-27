module Unfold exposing (..)

import Expect exposing (..)
import Fuzz exposing (..)
import Test exposing (..)
import Folding.Unfold as Unfold exposing (Unfold)


listOps : Test
listOps =
  describe "Comparison to list ops"
    [
      fuzz (list string) "concat" <| \ stringList ->
        equal (String.concat stringList)
          (Unfold.toString (Unfold.concat (List.map Unfold.string stringList)))
      ,
      fuzz (list int) "map" <| \ intList ->
        equal (List.map (\ x -> x + 1) intList)
          (Unfold.toList (Unfold.map (\ x -> x + 1) (Unfold.list intList)))
      ,
      fuzz2 (list int) (list int) "prepend" <| \ a b ->
        equal (List.append a b)
          (Unfold.toList (Unfold.prepend (Unfold.list a) (Unfold.list b)))
      ,
      fuzz2 int (list int) "cons" <| \ a b ->
        equal (a :: b)
          (Unfold.toList (Unfold.cons a (Unfold.list b)))
      ,
      fuzz2 int (list int) "snoc" <| \ a b ->
        equal (b ++ [a])
          (Unfold.toList (Unfold.snoc a (Unfold.list b)))
      ,
      fuzz2 (intRange -100 100) (intRange -100 100) "range" <| \ a b ->
        equal (List.range a b)
          (Unfold.toList (Unfold.range a b))
    ]
