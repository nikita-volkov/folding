module Unfoldl exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner
import Folding.Unfoldl as Unfoldl exposing (Unfoldl)


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
      ]
