module Test.Retry where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Flow.Core as Flow.Core
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Test.Util as Test.Util

spec :: Test.Spec.Spec Unit
spec = do
  Test.Spec.it "defaultRetryPolicy has expected defaults" do
    Flow.Core.defaultRetryPolicy `Test.Spec.Assertions.shouldEqual`
      ( Flow.Types.RetryPolicy
          { maxAttempts: 3
          , initialDelay: Flow.Types.Milliseconds 1000
          , backoffMultiplier: 2.0
          , maxDelay: Flow.Types.Milliseconds 30000
          }
      )

  Test.BDD.feature "Retry diagrams" do
    Test.BDD.scenario "simple retry" do
      Test.Spec.it "contains subgraph" do
        let output = testSimpleRetry
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "subgraph"
      Test.Spec.it "contains Retry label" do
        let output = testSimpleRetry
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Retry"
      Test.Spec.it "contains 3x attempts" do
        let output = testSimpleRetry
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "3x"
      Test.Spec.it "contains step name FallibleOp" do
        let output = testSimpleRetry
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "FallibleOp"

    Test.BDD.scenario "retry with custom policy" do
      Test.Spec.it "contains Retry label" do
        let output = testRetryWithCustomPolicy
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Retry"
      Test.Spec.it "contains 5x attempts" do
        let output = testRetryWithCustomPolicy
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "5x"
      Test.Spec.it "contains step name NetworkCall" do
        let output = testRetryWithCustomPolicy
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "NetworkCall"

    Test.BDD.scenario "retry with sequential inner workflow" do
      Test.Spec.it "contains Retry label" do
        let output = testRetryWithSequentialInner
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Retry"
      Test.Spec.it "contains 3x attempts" do
        let output = testRetryWithSequentialInner
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "3x"
      Test.Spec.it "contains step Prepare" do
        let output = testRetryWithSequentialInner
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Prepare"
      Test.Spec.it "contains step Execute" do
        let output = testRetryWithSequentialInner
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Execute"

    Test.BDD.scenario "nested retry" do
      Test.Spec.it "produces multiple Retry subgraphs" do
        let output = testNestedRetry
        let retryCount = Test.Util.countOccurrences "Retry" output
        (retryCount >= 2) `Test.Spec.Assertions.shouldEqual` true
      Test.Spec.it "contains inner 2x attempts" do
        let output = testNestedRetry
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "2x"
      Test.Spec.it "contains outer 3x attempts" do
        let output = testNestedRetry
        output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "3x"

  Test.BDD.feature "Retry execution" do
    Test.BDD.scenario "Identity monad" do
      Test.Spec.it "immediate success returns Right with 1 attempt" do
        let
          alwaysSucceedW :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
          alwaysSucceedW = Flow.Types.Pure (\n -> Data.Either.Right (n * 2))

          retriedW :: Flow.Types.Workflow () () Int (Flow.Types.RetryResult String Int)
          retriedW = Flow.Core.retry Flow.Core.defaultRetryPolicy alwaysSucceedW

          retryResult = Test.Util.runPure retriedW 5
        retryResult `Test.Spec.Assertions.shouldEqual`
          (Flow.Types.RetryResult { attempts: 1, result: Data.Either.Right 10 })

      Test.Spec.it "permanent failure exhausts maxAttempts" do
        let
          alwaysFailW :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
          alwaysFailW = Flow.Types.Pure (\_ -> Data.Either.Left "error")

          policy :: Flow.Types.RetryPolicy
          policy = Flow.Types.RetryPolicy
            { maxAttempts: 3
            , initialDelay: Flow.Types.Milliseconds 100
            , backoffMultiplier: 2.0
            , maxDelay: Flow.Types.Milliseconds 1000
            }

          retriedW :: Flow.Types.Workflow () () Int (Flow.Types.RetryResult String Int)
          retriedW = Flow.Core.retry policy alwaysFailW

          retryResult = Test.Util.runPure retriedW 5
        retryResult `Test.Spec.Assertions.shouldEqual`
          (Flow.Types.RetryResult { attempts: 3, result: Data.Either.Left "error" })

    Test.BDD.scenario "calculateBackoff" do
      Test.Spec.it "initial delay at attempt 1 equals initialDelay" do
        let
          policy :: Flow.Types.RetryPolicy
          policy = Flow.Types.RetryPolicy
            { maxAttempts: 3
            , initialDelay: Flow.Types.Milliseconds 1000
            , backoffMultiplier: 2.0
            , maxDelay: Flow.Types.Milliseconds 30000
            }
          result = Flow.Interpret.Effect.calculateBackoff policy 1
        result `Test.Spec.Assertions.shouldEqual` Flow.Types.Milliseconds 1000

      Test.Spec.it "exponential growth at attempt 2 equals initialDelay * backoffMultiplier" do
        let
          policy :: Flow.Types.RetryPolicy
          policy = Flow.Types.RetryPolicy
            { maxAttempts: 3
            , initialDelay: Flow.Types.Milliseconds 1000
            , backoffMultiplier: 2.0
            , maxDelay: Flow.Types.Milliseconds 30000
            }
          result = Flow.Interpret.Effect.calculateBackoff policy 2
        result `Test.Spec.Assertions.shouldEqual` Flow.Types.Milliseconds 2000

      Test.Spec.it "clamps to maxDelay when calculated delay exceeds it" do
        let
          policy :: Flow.Types.RetryPolicy
          policy = Flow.Types.RetryPolicy
            { maxAttempts: 10
            , initialDelay: Flow.Types.Milliseconds 1000
            , backoffMultiplier: 10.0
            , maxDelay: Flow.Types.Milliseconds 5000
            }
          result = Flow.Interpret.Effect.calculateBackoff policy 3
        result `Test.Spec.Assertions.shouldEqual` Flow.Types.Milliseconds 5000

testSimpleRetry :: String
testSimpleRetry =
  let
    fallibleW :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    fallibleW = Flow.Types.Step "FallibleOp" (Flow.Types.Pure Data.Either.Right)

    retriedW :: Flow.Types.Workflow () () Int (Flow.Types.RetryResult String Int)
    retriedW = Flow.Core.retry Flow.Core.defaultRetryPolicy fallibleW
  in
    Flow.Interpret.Diagram.toMermaid retriedW

testRetryWithCustomPolicy :: String
testRetryWithCustomPolicy =
  let
    policy :: Flow.Types.RetryPolicy
    policy = Flow.Types.RetryPolicy
      { maxAttempts: 5
      , initialDelay: Flow.Types.Milliseconds 500
      , backoffMultiplier: 1.5
      , maxDelay: Flow.Types.Milliseconds 10000
      }

    fallibleW :: Flow.Types.Workflow () () String (Data.Either.Either String String)
    fallibleW = Flow.Types.Step "NetworkCall" (Flow.Types.Pure Data.Either.Right)

    retriedW :: Flow.Types.Workflow () () String (Flow.Types.RetryResult String String)
    retriedW = Flow.Core.retry policy fallibleW
  in
    Flow.Interpret.Diagram.toMermaid retriedW

testRetryWithSequentialInner :: String
testRetryWithSequentialInner =
  let
    firstW :: Flow.Types.Workflow () () Int Int
    firstW = Flow.Types.Step "Prepare" (Flow.Types.Pure (_ * 2))

    secondW :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    secondW = Flow.Types.Step "Execute" (Flow.Types.Pure Data.Either.Right)

    sequentialW :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    sequentialW = firstW Control.Category.>>> secondW

    policy :: Flow.Types.RetryPolicy
    policy = Flow.Types.RetryPolicy
      { maxAttempts: 3
      , initialDelay: Flow.Types.Milliseconds 1000
      , backoffMultiplier: 2.0
      , maxDelay: Flow.Types.Milliseconds 30000
      }

    retriedW :: Flow.Types.Workflow () () Int (Flow.Types.RetryResult String Int)
    retriedW = Flow.Core.retry policy sequentialW
  in
    Flow.Interpret.Diagram.toMermaid retriedW

testNestedRetry :: String
testNestedRetry =
  let
    innerFallibleW :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    innerFallibleW = Flow.Types.Step "Inner" (Flow.Types.Pure Data.Either.Right)

    innerPolicy :: Flow.Types.RetryPolicy
    innerPolicy = Flow.Types.RetryPolicy
      { maxAttempts: 2
      , initialDelay: Flow.Types.Milliseconds 100
      , backoffMultiplier: 2.0
      , maxDelay: Flow.Types.Milliseconds 1000
      }

    innerRetriedW :: Flow.Types.Workflow () () Int (Flow.Types.RetryResult String Int)
    innerRetriedW = Flow.Core.retry innerPolicy innerFallibleW

    wrapResultW :: Flow.Types.Workflow () () (Flow.Types.RetryResult String Int) (Data.Either.Either String Int)
    wrapResultW = Flow.Types.Pure (\(Flow.Types.RetryResult r) -> r.result)

    outerFallibleW :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    outerFallibleW = innerRetriedW Control.Category.>>> wrapResultW

    outerPolicy :: Flow.Types.RetryPolicy
    outerPolicy = Flow.Types.RetryPolicy
      { maxAttempts: 3
      , initialDelay: Flow.Types.Milliseconds 500
      , backoffMultiplier: 2.0
      , maxDelay: Flow.Types.Milliseconds 5000
      }

    outerRetriedW :: Flow.Types.Workflow () () Int (Flow.Types.RetryResult String Int)
    outerRetriedW = Flow.Core.retry outerPolicy outerFallibleW
  in
    Flow.Interpret.Diagram.toMermaid outerRetriedW
