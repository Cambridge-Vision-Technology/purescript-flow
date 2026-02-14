module Test.Util where

import Prelude

import Data.Array as Data.Array
import Data.String as Data.String
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types

runPure :: forall a b. Flow.Types.Workflow () () a b -> a -> b
runPure = Flow.Interpret.Effect.runWorkflow

containsPattern :: String -> String -> Boolean
containsPattern pattern str = Data.String.contains (Data.String.Pattern pattern) str

countOccurrences :: String -> String -> Int
countOccurrences needle haystack =
  let
    parts = Data.String.split (Data.String.Pattern needle) haystack
  in
    Data.Array.length parts - 1

doubleW :: forall i o. Flow.Types.Workflow i o Int Int
doubleW = Flow.Types.Pure (_ * 2)

addTenW :: forall i o. Flow.Types.Workflow i o Int Int
addTenW = Flow.Types.Pure (_ + 10)

showW :: forall i o. Flow.Types.Workflow i o Int String
showW = Flow.Types.Pure show

negateW :: forall i o. Flow.Types.Workflow i o Int Int
negateW = Flow.Types.Pure negate
