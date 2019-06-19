module Unfoldl exposing (..)

import Expect exposing (..)
import Fuzz exposing (..)
import Test exposing (..)
import Folding.Unfoldl as Unfoldl exposing (Unfoldl)


listOps : Test
listOps =
  describe "Comparison to list ops"
    [
      fuzz (list string) "concat" <| \ stringList ->
        equal (String.concat stringList)
          (Unfoldl.toString (Unfoldl.concat (List.map Unfoldl.string stringList)))
      ,
      fuzz (list int) "map" <| \ intList ->
        equal (List.map (\ x -> x + 1) intList)
          (Unfoldl.toList (Unfoldl.map (\ x -> x + 1) (Unfoldl.list intList)))
      ,
      fuzz2 (list int) (list int) "prepend" <| \ a b ->
        equal (List.append a b)
          (Unfoldl.toList (Unfoldl.prepend (Unfoldl.list a) (Unfoldl.list b)))
      ,
      fuzz2 int (list int) "cons" <| \ a b ->
        equal (a :: b)
          (Unfoldl.toList (Unfoldl.cons a (Unfoldl.list b)))
      ,
      fuzz2 int (list int) "snoc" <| \ a b ->
        equal (b ++ [a])
          (Unfoldl.toList (Unfoldl.snoc a (Unfoldl.list b)))
      ,
      fuzz2 (intRange -100 100) (intRange -100 100) "range" <| \ a b ->
        equal (List.range a b)
          (Unfoldl.toList (Unfoldl.range a b))
    ]
