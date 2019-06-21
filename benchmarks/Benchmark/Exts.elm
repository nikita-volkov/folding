module Benchmark.Exts exposing (..)
{-| Extensions, which let us isolate naming from definitions. -}

import Benchmark exposing (..)


type alias BenchmarkDefinition = String -> Benchmark

define : String -> BenchmarkDefinition -> Benchmark
define = (|>)

comparison : String -> (() -> a) -> String -> (() -> b) -> BenchmarkDefinition
comparison lName lFn rName rFn name_ = Benchmark.compare name_ lName lFn rName rFn
