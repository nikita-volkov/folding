module Unfold exposing (..)

import Benchmark exposing (..)
import Benchmark.Exts exposing (..)
import Benchmark.Runner
import Folding.Unfold as Unfold exposing (Unfold)
import Set
import Array


main =
  Benchmark.Runner.program <|
    describe "Comparison" <|
      [
        describe "map" <|
          let
            sample = List.range 0 1000
            op x = x + 1
            in
              [
                Benchmark.compare "single"
                  "List" (\ _ -> List.map op sample)
                  "Unfold" (\ _ -> Unfold.toList (Unfold.map op (Unfold.list sample)))
                ,
                Benchmark.compare "double"
                  "List" (\ _ -> List.map op (List.map op sample))
                  "Unfold" (\ _ -> Unfold.toList (Unfold.map op (Unfold.map op (Unfold.list sample))))
                ,
                Benchmark.compare "triple"
                  "List" (\ _ -> List.map op (List.map op (List.map op sample)))
                  "Unfold" (\ _ -> Unfold.toList (Unfold.map op (Unfold.map op (Unfold.map op (Unfold.list sample)))))
                ,
                Benchmark.compare "quadruple"
                  "List" (\ _ -> List.map op (List.map op (List.map op (List.map op sample))))
                  "Unfold" (\ _ -> Unfold.toList (Unfold.map op (Unfold.map op (Unfold.map op (Unfold.map op (Unfold.list sample))))))
              ]
        ,
        describe "concat" <|
          let
            bySize size =
              let
                sample = List.range 0 100 |> List.repeat size
                in
                  Benchmark.compare (String.fromInt size)
                    "List" (\ _ -> List.concat sample)
                    "Unfold" (\ _ -> Unfold.toList (Unfold.joinMap Unfold.list (Unfold.list sample)))
            in
              [
                bySize 10
              ]
        ,
        let
          sample = List.range 0 100 |> List.repeat 10
          op x = x + 1
          in
            Benchmark.compare "concatMap & map"
              "List" (\ _ -> List.concatMap (List.map op) sample)
              "Unfold" (\ _ -> Unfold.toList (Unfold.joinMap (Unfold.map op << Unfold.list) (Unfold.list sample)))
        ,
        describe "append" <|
          let
            sample = List.range 0 100
            in
              [
                Benchmark.compare "single"
                  "List" (\ _ -> sample ++ sample)
                  "Unfold" (\ _ -> Unfold.toList (Unfold.prepend (Unfold.list sample) (Unfold.list sample)))
                ,
                Benchmark.compare "double"
                  "List" (\ _ -> sample ++ sample ++ sample)
                  "Unfold" (\ _ -> Unfold.toList (Unfold.prepend (Unfold.list sample) (Unfold.prepend (Unfold.list sample) (Unfold.list sample))))
                ,
                Benchmark.compare "triple"
                  "List" (\ _ -> sample ++ sample ++ sample ++ sample)
                  "Unfold" (\ _ -> Unfold.toList (Unfold.prepend (Unfold.list sample) (Unfold.prepend (Unfold.list sample) (Unfold.prepend (Unfold.list sample) (Unfold.list sample)))))
              ]
        ,
        define "map and convert to set" <|
          let
            sample = List.range 0 1000
            op x = x + 1
            in
              comparison
                "List" (\ _ -> Set.fromList (List.map op sample))
                "Unfold" (\ _ -> Unfold.toSet (Unfold.map op (Unfold.list sample)))
        ,
        define "range to set" <|
          let
            op x = x + 1
            in comparison
              "List" (\ _ -> Set.fromList (List.range 0 100))
              "Unfold" (\ _ -> Unfold.toSet (Unfold.range 0 100))
        ,
        define "concat arrays into set" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            in comparison
              "List" (\ _ -> Set.fromList (List.concatMap Array.toList sample))
              "Unfold" (\ _ -> Unfold.toSet (Unfold.joinMap Unfold.array (Unfold.list sample)))
        ,
        define "map and concat arrays into set" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            op x = x + 1
            in comparison
              "List" (\ _ -> Set.fromList (List.concatMap (List.map op << Array.toList) sample))
              "Unfold" (\ _ -> Unfold.toSet (Unfold.joinMap (Unfold.map op << Unfold.array) (Unfold.list sample)))
        ,
        define "map and concat arrays into list regardless of order" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            op x = x + 1
            in comparison
              "List" (\ _ -> (List.concatMap (List.map op << Array.toList) sample))
              "Unfold" (\ _ -> Unfold.toReverseList (Unfold.joinMap (Unfold.map op << Unfold.array) (Unfold.list sample)))
        ,
        define "concat different datastructures into list regardless of order" <|
          let
            list = List.range 0 99
            array = Array.fromList (List.range 100 199)
            set = Set.fromList (List.range 200 299)
            in comparison
              "List" (\ _ -> List.concat [list, Array.toList array, Set.toList set])
              "Unfold" (\ _ -> Unfold.toReverseList (Unfold.concat [Unfold.list list, Unfold.array array, Unfold.set set]))
      ]
