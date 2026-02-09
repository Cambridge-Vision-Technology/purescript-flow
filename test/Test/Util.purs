module Test.Util where

import Prelude

import Data.Array as Data.Array
import Data.Functor.Variant as Data.Functor.Variant
import Data.Identity as Data.Identity
import Data.String as Data.String
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types

runPure :: forall a b. Flow.Types.Workflow () () a b -> a -> b
runPure w a =
  let
    handler :: Flow.Interpret.Effect.EffectHandler Data.Identity.Identity ()
    handler = Data.Functor.Variant.case_
    Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler w a
  in
    result

containsPattern :: String -> String -> Boolean
containsPattern pattern str = Data.String.contains (Data.String.Pattern pattern) str

countOccurrences :: String -> String -> Int
countOccurrences needle haystack =
  let
    parts = Data.String.split (Data.String.Pattern needle) haystack
  in
    Data.Array.length parts - 1
