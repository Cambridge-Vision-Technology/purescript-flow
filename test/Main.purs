module Test.Main where

import Prelude

import Effect as Effect
import Test.Compose as Test.Compose
import Test.Diagram as Test.Diagram
import Test.Effect as Test.Effect
import Test.Retry as Test.Retry
import Test.Spec.Reporter as Test.Spec.Reporter
import Test.Spec.Runner.Node as Test.Spec.Runner.Node
import Test.Timeout as Test.Timeout

main :: Effect.Effect Unit
main = Test.Spec.Runner.Node.runSpecAndExitProcess [ Test.Spec.Reporter.consoleReporter ] do
  Test.Compose.spec
  Test.Diagram.spec
  Test.Effect.spec
  Test.Retry.spec
  Test.Timeout.spec
