module Unfoldl exposing (..)

import Benchmark exposing (..)
import Benchmark.Exts exposing (..)
import Benchmark.Runner
import Folding.Unfoldl as Unfoldl exposing (Unfoldl)
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
                  "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.map op (Unfoldl.list sample)))
                ,
                Benchmark.compare "double"
                  "List" (\ _ -> List.map op (List.map op sample))
                  "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.map op (Unfoldl.map op (Unfoldl.list sample))))
                ,
                Benchmark.compare "triple"
                  "List" (\ _ -> List.map op (List.map op (List.map op sample)))
                  "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.map op (Unfoldl.map op (Unfoldl.map op (Unfoldl.list sample)))))
                ,
                Benchmark.compare "quadruple"
                  "List" (\ _ -> List.map op (List.map op (List.map op (List.map op sample))))
                  "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.map op (Unfoldl.map op (Unfoldl.map op (Unfoldl.map op (Unfoldl.list sample))))))
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
                    "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.joinMap Unfoldl.list (Unfoldl.list sample)))
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
              "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.joinMap (Unfoldl.map op << Unfoldl.list) (Unfoldl.list sample)))
        ,
        describe "append" <|
          let
            sample = List.range 0 100
            in
              [
                Benchmark.compare "single"
                  "List" (\ _ -> sample ++ sample)
                  "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.prepend (Unfoldl.list sample) (Unfoldl.list sample)))
                ,
                Benchmark.compare "double"
                  "List" (\ _ -> sample ++ sample ++ sample)
                  "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.prepend (Unfoldl.list sample) (Unfoldl.prepend (Unfoldl.list sample) (Unfoldl.list sample))))
                ,
                Benchmark.compare "triple"
                  "List" (\ _ -> sample ++ sample ++ sample ++ sample)
                  "Unfoldl" (\ _ -> Unfoldl.toList (Unfoldl.prepend (Unfoldl.list sample) (Unfoldl.prepend (Unfoldl.list sample) (Unfoldl.prepend (Unfoldl.list sample) (Unfoldl.list sample)))))
              ]
        ,
        define "map and convert to set" <|
          let
            sample = List.range 0 1000
            op x = x + 1
            in
              comparison
                "List" (\ _ -> Set.fromList (List.map op sample))
                "Unfoldl" (\ _ -> Unfoldl.toSet (Unfoldl.map op (Unfoldl.list sample)))
        ,
        define "range to set" <|
          let
            op x = x + 1
            in comparison
              "List" (\ _ -> Set.fromList (List.range 0 100))
              "Unfoldl" (\ _ -> Unfoldl.toSet (Unfoldl.range 0 100))
        ,
        define "concat arrays into set" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            in comparison
              "List" (\ _ -> Set.fromList (List.concatMap Array.toList sample))
              "Unfoldl" (\ _ -> Unfoldl.toSet (Unfoldl.joinMap Unfoldl.array (Unfoldl.list sample)))
        ,
        define "map and concat arrays into set" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            op x = x + 1
            in comparison
              "List" (\ _ -> Set.fromList (List.concatMap (List.map op << Array.toList) sample))
              "Unfoldl" (\ _ -> Unfoldl.toSet (Unfoldl.joinMap (Unfoldl.map op << Unfoldl.array) (Unfoldl.list sample)))
        ,
        define "map and concat arrays into list regardless of order" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            op x = x + 1
            in comparison
              "List" (\ _ -> (List.concatMap (List.map op << Array.toList) sample))
              "Unfoldl" (\ _ -> Unfoldl.toReverseList (Unfoldl.joinMap (Unfoldl.map op << Unfoldl.array) (Unfoldl.list sample)))
        ,
        define "concat different datastructures into list regardless of order" <|
          let
            list = List.range 0 99
            array = Array.fromList (List.range 100 199)
            set = Set.fromList (List.range 200 299)
            in comparison
              "List" (\ _ -> List.concat [list, Array.toList array, Set.toList set])
              "Unfoldl" (\ _ -> Unfoldl.toReverseList (Unfoldl.concat [Unfoldl.list list, Unfoldl.array array, Unfoldl.set set]))
      ]
