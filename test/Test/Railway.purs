module Test.Railway where

import Prelude

import Data.Either as Data.Either
import Data.Profunctor.Choice as Data.Profunctor.Choice
import Flow.Core as Flow.Core
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Util as Test.Util
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

spec :: Test.Spec.Spec Unit
spec = do
  executionSpec
  diagramSpec
  integrationSpec

executionSpec :: Test.Spec.Spec Unit
executionSpec = Test.BDD.feature "Railway (composeK)" do
  Test.BDD.scenario "two workflows, all Right path succeeds" do
    Test.Spec.it "chains two successful steps" do
      let
        step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step1 = Flow.Types.Pure (\n -> Data.Either.Right (n * 2))

        step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step2 = Flow.Types.Pure (\n -> Data.Either.Right (n + 10))

        railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        railway = Flow.Core.composeK step1 step2

        result = Test.Util.runPure railway 5
      result `Test.Spec.Assertions.shouldEqual` (Data.Either.Right 20)

  Test.BDD.scenario "short-circuit at first step" do
    Test.Spec.it "Left at first step skips second" do
      let
        step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step1 = Flow.Types.Pure (\_ -> Data.Either.Left "error at step 1")

        step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step2 = Flow.Types.Pure (\n -> Data.Either.Right (n + 10))

        railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        railway = Flow.Core.composeK step1 step2

        result = Test.Util.runPure railway 5
      result `Test.Spec.Assertions.shouldEqual` (Data.Either.Left "error at step 1")

  Test.BDD.scenario "short-circuit at second step" do
    Test.Spec.it "Left at second step propagates the error" do
      let
        step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step1 = Flow.Types.Pure (\n -> Data.Either.Right (n * 2))

        step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step2 = Flow.Types.Pure (\_ -> Data.Either.Left "error at step 2")

        railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        railway = Flow.Core.composeK step1 step2

        result = Test.Util.runPure railway 5
      result `Test.Spec.Assertions.shouldEqual` (Data.Either.Left "error at step 2")

  Test.BDD.scenario "three workflows, all Right path" do
    Test.Spec.it "chains three successful steps" do
      let
        step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step1 = Flow.Types.Pure (\n -> Data.Either.Right (n * 2))

        step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step2 = Flow.Types.Pure (\n -> Data.Either.Right (n + 10))

        step3 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step3 = Flow.Types.Pure (\n -> Data.Either.Right (n * 3))

        railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        railway = Flow.Core.composeK step1 (Flow.Core.composeK step2 step3)

        result = Test.Util.runPure railway 5
      result `Test.Spec.Assertions.shouldEqual` (Data.Either.Right 60)

  Test.BDD.scenario "left-nested three workflows, all Right path" do
    Test.Spec.it "chains three successful steps with left nesting" do
      let
        step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step1 = Flow.Types.Pure (\n -> Data.Either.Right (n * 2))

        step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step2 = Flow.Types.Pure (\n -> Data.Either.Right (n + 10))

        step3 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step3 = Flow.Types.Pure (\n -> Data.Either.Right (n * 3))

        railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        railway = Flow.Core.composeK (Flow.Core.composeK step1 step2) step3

        result = Test.Util.runPure railway 5
      result `Test.Spec.Assertions.shouldEqual` (Data.Either.Right 60)

  Test.BDD.scenario "three workflows, middle step fails" do
    Test.Spec.it "Left at middle step skips the third" do
      let
        step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step1 = Flow.Types.Pure (\n -> Data.Either.Right (n * 2))

        step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step2 = Flow.Types.Pure (\_ -> Data.Either.Left "error at step 2")

        step3 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        step3 = Flow.Types.Pure (\n -> Data.Either.Right (n * 3))

        railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
        railway = Flow.Core.composeK step1 (Flow.Core.composeK step2 step3)

        result = Test.Util.runPure railway 5
      result `Test.Spec.Assertions.shouldEqual` (Data.Either.Left "error at step 2")

diagramSpec :: Test.Spec.Spec Unit
diagramSpec = Test.BDD.feature "Railway diagram rendering" do
  Test.BDD.scenario "two-step railway renders as subgraph with sequential steps and error rail" do
    Test.Spec.it "renders exact Mermaid output" do
      testTwoStepRailwayDiagram `Test.Spec.Assertions.shouldEqual`
        "flowchart TD\n    subgraph railway_N0 [Railway]\n    N1[Validate]\n    N2[Process]\n    N1 --> N2\n    end\n    N3([Error])\n    N1 -->|Err| N3\n    N2 -->|Err| N3"

  Test.BDD.scenario "three-step railway renders single subgraph with 3 steps and error rail" do
    Test.Spec.it "renders exact Mermaid output" do
      testThreeStepRailwayDiagram `Test.Spec.Assertions.shouldEqual`
        "flowchart TD\n    subgraph railway_N0 [Railway]\n    N1[Validate]\n    N2[Process]\n    N3[Save]\n    N1 --> N2\n    N2 --> N3\n    end\n    N4([Error])\n    N1 -->|Err| N4\n    N2 -->|Err| N4\n    N3 -->|Err| N4"

  Test.BDD.scenario "left-nested three-step railway renders single subgraph with 3 steps and error rail" do
    Test.Spec.it "renders exact Mermaid output" do
      testLeftNestedThreeStepRailwayDiagram `Test.Spec.Assertions.shouldEqual`
        "flowchart TD\n    subgraph railway_N0 [Railway]\n    N1[Validate]\n    N2[Process]\n    N3[Save]\n    N1 --> N2\n    N2 --> N3\n    end\n    N4([Error])\n    N1 -->|Err| N4\n    N2 -->|Err| N4\n    N3 -->|Err| N4"

  Test.BDD.scenario "standalone right still renders as Decision diamond" do
    Test.Spec.it "renders exact Mermaid output" do
      testStandaloneRightDiagram `Test.Spec.Assertions.shouldEqual`
        "flowchart TD\n    N0{Decision}\n    N2[Process]\n    N0 -->|Left| N1\n    N0 -->|Right| N2\n    N3[merge]\n    N1 --> N3\n    N2 --> N3"

testTwoStepRailwayDiagram :: String
testTwoStepRailwayDiagram =
  let
    step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step1 = Flow.Types.Step "Validate" (Flow.Types.Pure (\n -> Data.Either.Right (n * 2)))

    step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step2 = Flow.Types.Step "Process" (Flow.Types.Pure (\n -> Data.Either.Right (n + 10)))

    railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    railway = Flow.Core.composeK step1 step2
  in
    Flow.Interpret.Diagram.toMermaid railway

testThreeStepRailwayDiagram :: String
testThreeStepRailwayDiagram =
  let
    step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step1 = Flow.Types.Step "Validate" (Flow.Types.Pure (\n -> Data.Either.Right (n * 2)))

    step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step2 = Flow.Types.Step "Process" (Flow.Types.Pure (\n -> Data.Either.Right (n + 10)))

    step3 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step3 = Flow.Types.Step "Save" (Flow.Types.Pure (\n -> Data.Either.Right (n * 3)))

    railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    railway = Flow.Core.composeK step1 (Flow.Core.composeK step2 step3)
  in
    Flow.Interpret.Diagram.toMermaid railway

testLeftNestedThreeStepRailwayDiagram :: String
testLeftNestedThreeStepRailwayDiagram =
  let
    step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step1 = Flow.Types.Step "Validate" (Flow.Types.Pure (\n -> Data.Either.Right (n * 2)))

    step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step2 = Flow.Types.Step "Process" (Flow.Types.Pure (\n -> Data.Either.Right (n + 10)))

    step3 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step3 = Flow.Types.Step "Save" (Flow.Types.Pure (\n -> Data.Either.Right (n * 3)))

    railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    railway = Flow.Core.composeK (Flow.Core.composeK step1 step2) step3
  in
    Flow.Interpret.Diagram.toMermaid railway

testStandaloneRightDiagram :: String
testStandaloneRightDiagram =
  let
    inner :: Flow.Types.Workflow () () Int Int
    inner = Flow.Types.Step "Process" (Flow.Types.Pure (_ * 2))

    workflow :: Flow.Types.Workflow () () (Data.Either.Either String Int) (Data.Either.Either String Int)
    workflow = Data.Profunctor.Choice.right inner
  in
    Flow.Interpret.Diagram.toMermaid workflow

integrationSpec :: Test.Spec.Spec Unit
integrationSpec = do
  railwayInsideEncapDiagramSpec
  railwayWithTimeoutSpec

railwayInsideEncapDiagramSpec :: Test.Spec.Spec Unit
railwayInsideEncapDiagramSpec = Test.BDD.feature "Railway inside Encap" do
  Test.BDD.scenario "railway wrapped in encapsulation renders nested subgraphs" do
    Test.Spec.it "renders exact Mermaid output" do
      testRailwayInsideEncapDiagram `Test.Spec.Assertions.shouldEqual`
        "flowchart TD\n    subgraph encap_N0 [Auth Pipeline]\n    subgraph railway_N1 [Railway]\n    N2[Validate]\n    N3[Process]\n    N2 --> N3\n    end\n    N4([Error])\n    N2 -->|Err| N4\n    N3 -->|Err| N4\n    end"

railwayWithTimeoutSpec :: Test.Spec.Spec Unit
railwayWithTimeoutSpec = do
  Test.BDD.feature "Railway with Timeout diagram" do
    Test.BDD.scenario "timeout wrapping railway renders nested subgraphs" do
      Test.Spec.it "renders exact Mermaid output" do
        testTimeoutWrappingRailwayDiagram `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    subgraph timeout_N0 [Timeout 5000ms]\n    subgraph railway_N1 [Railway]\n    N2[Validate]\n    N3[Process]\n    N2 --> N3\n    end\n    N4([Error])\n    N2 -->|Err| N4\n    N3 -->|Err| N4\n    end"

  Test.BDD.feature "Railway with Timeout execution" do
    Test.BDD.scenario "timeout fires on railway with Identity monad" do
      Test.Spec.it "produces TimedOut error" do
        let
          step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
          step1 = Flow.Types.Pure (\n -> Data.Either.Right (n * 2))

          step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
          step2 = Flow.Types.Pure (\n -> Data.Either.Right (n + 10))

          railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
          railway = Flow.Core.composeK step1 step2

          timedRailway :: Flow.Types.Workflow () () Int (Data.Either.Either Flow.Types.TimeoutError (Data.Either.Either String Int))
          timedRailway = Flow.Core.timeout (Flow.Types.Milliseconds 5000) railway

          result = Test.Util.runPure timedRailway 5
        result `Test.Spec.Assertions.shouldEqual` Data.Either.Left (Flow.Types.TimedOut (Flow.Types.Milliseconds 5000))

testRailwayInsideEncapDiagram :: String
testRailwayInsideEncapDiagram =
  let
    step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step1 = Flow.Types.Step "Validate" (Flow.Types.Pure (\n -> Data.Either.Right (n * 2)))

    step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step2 = Flow.Types.Step "Process" (Flow.Types.Pure (\n -> Data.Either.Right (n + 10)))

    railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    railway = Flow.Core.composeK step1 step2

    encap :: Flow.Types.Encapsulation () () () ()
    encap = Flow.Types.Encapsulation { events: identity, messages: identity }

    workflow :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    workflow = Flow.Types.encapsulate "Auth Pipeline" encap railway
  in
    Flow.Interpret.Diagram.toMermaid workflow

testTimeoutWrappingRailwayDiagram :: String
testTimeoutWrappingRailwayDiagram =
  let
    step1 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step1 = Flow.Types.Step "Validate" (Flow.Types.Pure (\n -> Data.Either.Right (n * 2)))

    step2 :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    step2 = Flow.Types.Step "Process" (Flow.Types.Pure (\n -> Data.Either.Right (n + 10)))

    railway :: Flow.Types.Workflow () () Int (Data.Either.Either String Int)
    railway = Flow.Core.composeK step1 step2

    workflow :: Flow.Types.Workflow () () Int (Data.Either.Either Flow.Types.TimeoutError (Data.Either.Either String Int))
    workflow = Flow.Core.timeout (Flow.Types.Milliseconds 5000) railway
  in
    Flow.Interpret.Diagram.toMermaid workflow
