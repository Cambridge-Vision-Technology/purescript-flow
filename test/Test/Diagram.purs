module Test.Diagram where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.Tuple as Data.Tuple
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

spec :: Test.Spec.Spec Unit
spec = do
  Test.BDD.feature "Diagram generation" do
    Test.BDD.scenario "sequential workflow" do
      Test.Spec.it "renders exact Mermaid output" do
        testSequentialWorkflow `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    N0[Double]\n    N1[Add Ten]\n    N2[Show]\n    N1 --> N2\n    N0 --> N1"

    Test.BDD.scenario "parallel workflow" do
      Test.Spec.it "renders exact Mermaid output" do
        testParallelWorkflow `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    subgraph parallel_N0 [Parallel]\n    N1[Double]\n    N2[Negate]\n    end\n    N3[fork]\n    N4[join]\n    N3 --> N1\n    N3 --> N2\n    N1 --> N4\n    N2 --> N4"

    Test.BDD.scenario "choice workflow" do
      Test.Spec.it "renders exact Mermaid output" do
        testChoiceWorkflow `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    N0{Decision}\n    N1[Handle Left]\n    N2[Handle Right]\n    N0 -->|Left| N1\n    N0 -->|Right| N2\n    N3[merge]\n    N1 --> N3\n    N2 --> N3"

    Test.BDD.scenario "mapArray workflow" do
      Test.Spec.it "renders exact Mermaid output" do
        testMapArrayWorkflow `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    subgraph foreach_N0 [For Each]\n    N1[Process Item]\n    end"

    Test.BDD.scenario "single-branch parallel" do
      Test.Spec.it "renders exact Mermaid output" do
        testSingleBranchParFirst `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    N0[Double]"

    Test.BDD.scenario "pure nodes skipped in sequences" do
      Test.Spec.it "renders exact Mermaid output" do
        testPureSkippedInSeq `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    N0[Double]\n    N1[Add Ten]\n    N0 --> N1"

  Test.BDD.feature "Leaf Diagrams" do
    Test.BDD.scenario "single leaf renders labeled node" do
      Test.Spec.it "renders exact Mermaid output" do
        testSingleLeaf `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    N0[Authenticate]"

    Test.BDD.scenario "sequential leaves connected by arrows" do
      Test.Spec.it "renders exact Mermaid output" do
        testSequentialLeaves `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    N0[Validate]\n    N1[Process]\n    N0 --> N1"

  Test.BDD.feature "Encapsulation Diagrams" do
    Test.BDD.scenario "encapsulated workflow renders as labeled subgraph" do
      Test.Spec.it "renders exact Mermaid output" do
        testEncapsulatedLeaf `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    subgraph encap_N0 [Domain Layer]\n    N1[Inner Task]\n    end"

    Test.BDD.scenario "nested encapsulation renders nested subgraphs" do
      Test.Spec.it "renders exact Mermaid output" do
        testNestedEncapsulation `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    subgraph encap_N0 [Outer Encap]\n    subgraph encap_N1 [Inner Encap]\n    N2[Core Logic]\n    end\n    end"

    Test.BDD.scenario "(~>) operator renders without subgraph wrapper" do
      Test.Spec.it "renders exact Mermaid output" do
        testEncapWithTildeArrow `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    N0[Authenticate]"

    Test.BDD.scenario "full stack (domain inside platform inside harness)" do
      Test.Spec.it "renders exact Mermaid output" do
        testFullStackEncapsulation `Test.Spec.Assertions.shouldEqual`
          "flowchart TD\n    subgraph encap_N0 [Test Harness]\n    subgraph encap_N1 [Platform]\n    subgraph encap_N2 [Domain]\n    N3[Business Logic]\n    end\n    end\n    end"

testSequentialWorkflow :: String
testSequentialWorkflow =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))

    addTenW :: Flow.Types.Workflow () () Int Int
    addTenW = Flow.Types.Step "Add Ten" (Flow.Types.Pure (_ + 10))

    showW :: Flow.Types.Workflow () () Int String
    showW = Flow.Types.Step "Show" (Flow.Types.Pure show)

    workflow :: Flow.Types.Workflow () () Int String
    workflow = doubleW Control.Category.>>> addTenW Control.Category.>>> showW
  in
    Flow.Interpret.Diagram.toMermaid workflow

testParallelWorkflow :: String
testParallelWorkflow =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))

    negateW :: Flow.Types.Workflow () () Int Int
    negateW = Flow.Types.Step "Negate" (Flow.Types.Pure negate)

    workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int Int) (Data.Tuple.Tuple Int Int)
    workflow = Flow.Types.mkPar doubleW negateW
  in
    Flow.Interpret.Diagram.toMermaid workflow

testChoiceWorkflow :: String
testChoiceWorkflow =
  let
    handleLeft :: Flow.Types.Workflow () () Int String
    handleLeft = Flow.Types.Step "Handle Left" (Flow.Types.Pure show)

    handleRight :: Flow.Types.Workflow () () String String
    handleRight = Flow.Types.Step "Handle Right" (Flow.Types.Pure identity)

    workflow :: Flow.Types.Workflow () () (Data.Either.Either Int String) String
    workflow = Flow.Types.mkChoice identity handleLeft handleRight
  in
    Flow.Interpret.Diagram.toMermaid workflow

testMapArrayWorkflow :: String
testMapArrayWorkflow =
  let
    innerW :: Flow.Types.Workflow () () Int Int
    innerW = Flow.Types.Step "Process Item" (Flow.Types.Pure (_ * 2))

    workflow :: Flow.Types.Workflow () () (Array Int) (Array Int)
    workflow = Flow.Types.mkMapArray innerW
  in
    Flow.Interpret.Diagram.toMermaid workflow

testSingleBranchParFirst :: String
testSingleBranchParFirst =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))

    workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple Int String)
    workflow = Data.Profunctor.Strong.first doubleW
  in
    Flow.Interpret.Diagram.toMermaid workflow

testPureSkippedInSeq :: String
testPureSkippedInSeq =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))

    addTenW :: Flow.Types.Workflow () () Int Int
    addTenW = Flow.Types.Step "Add Ten" (Flow.Types.Pure (_ + 10))

    workflow :: Flow.Types.Workflow () () Int Int
    workflow = doubleW Control.Category.>>> addTenW
  in
    Flow.Interpret.Diagram.toMermaid workflow

type PING r = (ping :: Unit | r)

type PONG r = (pong :: Unit | r)

simpleLeaf :: String -> Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
simpleLeaf label = Flow.Types.mkLeaf label
  (\_ -> Flow.Types.LeafDone unit)
  (\_ _ -> Flow.Types.LeafDone unit)

simpleEncap :: Flow.Types.Encapsulation (PING ()) (PONG ()) (PING ()) (PONG ())
simpleEncap = Flow.Types.Encapsulation
  { events: identity
  , messages: identity
  }

testSingleLeaf :: String
testSingleLeaf =
  Flow.Interpret.Diagram.toMermaid (simpleLeaf "Authenticate")

testSequentialLeaves :: String
testSequentialLeaves =
  let
    w1 :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    w1 = simpleLeaf "Validate"

    w2 :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    w2 = simpleLeaf "Process"

    workflow :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    workflow = w1 Control.Category.>>> w2
  in
    Flow.Interpret.Diagram.toMermaid workflow

testEncapsulatedLeaf :: String
testEncapsulatedLeaf =
  let
    inner :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    inner = simpleLeaf "Inner Task"

    workflow :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    workflow = Flow.Types.encapsulate "Domain Layer" simpleEncap inner
  in
    Flow.Interpret.Diagram.toMermaid workflow

testNestedEncapsulation :: String
testNestedEncapsulation =
  let
    inner :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    inner = simpleLeaf "Core Logic"

    once :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    once = Flow.Types.encapsulate "Inner Encap" simpleEncap inner

    twice :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    twice = Flow.Types.encapsulate "Outer Encap" simpleEncap once
  in
    Flow.Interpret.Diagram.toMermaid twice

testEncapWithTildeArrow :: String
testEncapWithTildeArrow =
  let
    inner :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    inner = simpleLeaf "Authenticate"

    workflow :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    workflow = inner Flow.Types.~> simpleEncap
  in
    Flow.Interpret.Diagram.toMermaid workflow

testFullStackEncapsulation :: String
testFullStackEncapsulation =
  let
    inner :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    inner = simpleLeaf "Business Logic"

    domain :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    domain = Flow.Types.encapsulate "Domain" simpleEncap inner

    platform :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    platform = Flow.Types.encapsulate "Platform" simpleEncap domain

    harness :: Flow.Types.Workflow (PING ()) (PONG ()) Unit Unit
    harness = Flow.Types.encapsulate "Test Harness" simpleEncap platform
  in
    Flow.Interpret.Diagram.toMermaid harness
