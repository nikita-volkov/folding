module Spread exposing (..)

import Benchmark exposing (..)
import Benchmark.Exts exposing (..)
import Benchmark.Runner
import Folding.Spread as Spread exposing (Spread)
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
                  "Spread" (\ _ -> Spread.toList (Spread.map op (Spread.list sample)))
                ,
                Benchmark.compare "double"
                  "List" (\ _ -> List.map op (List.map op sample))
                  "Spread" (\ _ -> Spread.toList (Spread.map op (Spread.map op (Spread.list sample))))
                ,
                Benchmark.compare "triple"
                  "List" (\ _ -> List.map op (List.map op (List.map op sample)))
                  "Spread" (\ _ -> Spread.toList (Spread.map op (Spread.map op (Spread.map op (Spread.list sample)))))
                ,
                Benchmark.compare "quadruple"
                  "List" (\ _ -> List.map op (List.map op (List.map op (List.map op sample))))
                  "Spread" (\ _ -> Spread.toList (Spread.map op (Spread.map op (Spread.map op (Spread.map op (Spread.list sample))))))
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
                    "Spread" (\ _ -> Spread.toList (Spread.joinMap Spread.list (Spread.list sample)))
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
              "Spread" (\ _ -> Spread.toList (Spread.joinMap (Spread.map op << Spread.list) (Spread.list sample)))
        ,
        describe "append" <|
          let
            sample = List.range 0 100
            in
              [
                Benchmark.compare "single"
                  "List" (\ _ -> sample ++ sample)
                  "Spread" (\ _ -> Spread.toList (Spread.prepend (Spread.list sample) (Spread.list sample)))
                ,
                Benchmark.compare "double"
                  "List" (\ _ -> sample ++ sample ++ sample)
                  "Spread" (\ _ -> Spread.toList (Spread.prepend (Spread.list sample) (Spread.prepend (Spread.list sample) (Spread.list sample))))
                ,
                Benchmark.compare "triple"
                  "List" (\ _ -> sample ++ sample ++ sample ++ sample)
                  "Spread" (\ _ -> Spread.toList (Spread.prepend (Spread.list sample) (Spread.prepend (Spread.list sample) (Spread.prepend (Spread.list sample) (Spread.list sample)))))
              ]
        ,
        define "map and convert to set" <|
          let
            sample = List.range 0 1000
            op x = x + 1
            in
              comparison
                "List" (\ _ -> Set.fromList (List.map op sample))
                "Spread" (\ _ -> Spread.toSet (Spread.map op (Spread.list sample)))
        ,
        define "range to set" <|
          let
            op x = x + 1
            in comparison
              "List" (\ _ -> Set.fromList (List.range 0 100))
              "Spread" (\ _ -> Spread.toSet (Spread.range 0 100))
        ,
        define "concat arrays into set" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            in comparison
              "List" (\ _ -> Set.fromList (List.concatMap Array.toList sample))
              "Spread" (\ _ -> Spread.toSet (Spread.joinMap Spread.array (Spread.list sample)))
        ,
        define "map and concat arrays into set" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            op x = x + 1
            in comparison
              "List" (\ _ -> Set.fromList (List.concatMap (List.map op << Array.toList) sample))
              "Spread" (\ _ -> Spread.toSet (Spread.joinMap (Spread.map op << Spread.array) (Spread.list sample)))
        ,
        define "map and concat arrays into list regardless of order" <|
          let
            sample = List.repeat 10 (Array.fromList (List.range 0 100))
            op x = x + 1
            in comparison
              "List" (\ _ -> (List.concatMap (List.map op << Array.toList) sample))
              "Spread" (\ _ -> Spread.toReverseList (Spread.joinMap (Spread.map op << Spread.array) (Spread.list sample)))
        ,
        define "concat different datastructures into list regardless of order" <|
          let
            list = List.range 0 99
            array = Array.fromList (List.range 100 199)
            set = Set.fromList (List.range 200 299)
            in comparison
              "List" (\ _ -> List.concat [list, Array.toList array, Set.toList set])
              "Spread" (\ _ -> Spread.toReverseList (Spread.concat [Spread.list list, Spread.array array, Spread.set set]))
      ]
