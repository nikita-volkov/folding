module Unfoldl exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner
import Folding.Unfoldl as Unfoldl exposing (Unfoldl)
import Set


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
      ]


-- # Extensions, which let us isolate naming from definitions

type alias BenchmarkDefinition = String -> Benchmark

define : String -> BenchmarkDefinition -> Benchmark
define = (|>)

comparison : String -> (() -> a) -> String -> (() -> b) -> BenchmarkDefinition
comparison lName lFn rName rFn name_ = Benchmark.compare name_ lName lFn rName rFn
