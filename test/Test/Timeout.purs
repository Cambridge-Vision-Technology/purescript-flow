module Test.Timeout where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Flow.Core as Flow.Core
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Test.Util as Test.Util

spec :: Test.Spec.Spec Unit
spec = do
  Test.BDD.feature "Timeout diagrams" do
    Test.BDD.scenario "simple timeout" do
      Test.Spec.it "contains subgraph" do
        let output = testSimpleTimeout
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "subgraph"
      Test.Spec.it "contains Timeout label" do
        let output = testSimpleTimeout
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Timeout"
      Test.Spec.it "contains duration 5000ms" do
        let output = testSimpleTimeout
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "5000ms"
      Test.Spec.it "contains step name SlowOp" do
        let output = testSimpleTimeout
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "SlowOp"

    Test.BDD.scenario "timeout with different duration" do
      Test.Spec.it "contains Timeout label" do
        let output = testTimeoutWithDifferentDuration
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Timeout"
      Test.Spec.it "contains duration 100ms" do
        let output = testTimeoutWithDifferentDuration
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "100ms"
      Test.Spec.it "contains step name ParseData" do
        let output = testTimeoutWithDifferentDuration
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "ParseData"

    Test.BDD.scenario "nested timeout" do
      Test.Spec.it "produces multiple Timeout subgraphs" do
        let output = testNestedTimeout
        let timeoutCount = Test.Util.countOccurrences "Timeout" output
        (timeoutCount >= 2) `Test.Spec.Assertions.shouldEqual` true
      Test.Spec.it "contains inner duration 1000ms" do
        let output = testNestedTimeout
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "1000ms"
      Test.Spec.it "contains outer duration 5000ms" do
        let output = testNestedTimeout
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "5000ms"

    Test.BDD.scenario "timeout wrapping sequential workflow" do
      Test.Spec.it "contains Timeout label" do
        let output = testTimeoutSequential
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Timeout"
      Test.Spec.it "contains duration 3000ms" do
        let output = testTimeoutSequential
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "3000ms"
      Test.Spec.it "contains step First" do
        let output = testTimeoutSequential
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "First"
      Test.Spec.it "contains step Second" do
        let output = testTimeoutSequential
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Second"

  Test.BDD.feature "Timeout execution" do
    Test.BDD.scenario "Identity monad" do
      Test.Spec.it "timeout always fires with Identity monad" do
        let
          innerW :: Flow.Types.Workflow () () Int Int
          innerW = Flow.Types.Pure (_ * 2)

          timedW :: Flow.Types.Workflow () () Int (Data.Either.Either Flow.Types.TimeoutError Int)
          timedW = Flow.Core.timeout (Flow.Types.Milliseconds 5000) innerW

          result = Test.Util.runPure timedW 42
        result `Test.Spec.Assertions.shouldEqual` Data.Either.Left (Flow.Types.TimedOut (Flow.Types.Milliseconds 5000))

testSimpleTimeout :: String
testSimpleTimeout =
  let
    workflow :: Flow.Types.Workflow () () Int Int
    workflow = Flow.Types.Step "SlowOp" (Flow.Types.Pure (_ * 2))

    timedWorkflow :: Flow.Types.Workflow () () Int (Data.Either.Either Flow.Types.TimeoutError Int)
    timedWorkflow = Flow.Core.timeout (Flow.Types.Milliseconds 5000) workflow
  in
    Flow.Interpret.Diagram.toMermaid timedWorkflow

testTimeoutWithDifferentDuration :: String
testTimeoutWithDifferentDuration =
  let
    workflow :: Flow.Types.Workflow () () String String
    workflow = Flow.Types.Step "ParseData" (Flow.Types.Pure identity)

    timedWorkflow :: Flow.Types.Workflow () () String (Data.Either.Either Flow.Types.TimeoutError String)
    timedWorkflow = Flow.Core.timeout (Flow.Types.Milliseconds 100) workflow
  in
    Flow.Interpret.Diagram.toMermaid timedWorkflow

testNestedTimeout :: String
testNestedTimeout =
  let
    innerW :: Flow.Types.Workflow () () Int Int
    innerW = Flow.Types.Step "Inner" (Flow.Types.Pure (_ + 1))

    innerTimedW :: Flow.Types.Workflow () () Int (Data.Either.Either Flow.Types.TimeoutError Int)
    innerTimedW = Flow.Core.timeout (Flow.Types.Milliseconds 1000) innerW

    handleTimeoutW :: Flow.Types.Workflow () () (Data.Either.Either Flow.Types.TimeoutError Int) Int
    handleTimeoutW = Flow.Types.Pure (Data.Either.either (\_ -> 0) identity)

    fullW :: Flow.Types.Workflow () () Int Int
    fullW = innerTimedW Control.Category.>>> handleTimeoutW

    outerTimedW :: Flow.Types.Workflow () () Int (Data.Either.Either Flow.Types.TimeoutError Int)
    outerTimedW = Flow.Core.timeout (Flow.Types.Milliseconds 5000) fullW
  in
    Flow.Interpret.Diagram.toMermaid outerTimedW

testTimeoutSequential :: String
testTimeoutSequential =
  let
    firstW :: Flow.Types.Workflow () () Int Int
    firstW = Flow.Types.Step "First" (Flow.Types.Pure (_ * 2))

    secondW :: Flow.Types.Workflow () () Int Int
    secondW = Flow.Types.Step "Second" (Flow.Types.Pure (_ + 10))

    sequentialW :: Flow.Types.Workflow () () Int Int
    sequentialW = firstW Control.Category.>>> secondW

    timedW :: Flow.Types.Workflow () () Int (Data.Either.Either Flow.Types.TimeoutError Int)
    timedW = Flow.Core.timeout (Flow.Types.Milliseconds 3000) sequentialW
  in
    Flow.Interpret.Diagram.toMermaid timedW
