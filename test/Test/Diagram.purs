module Test.Diagram where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Data.Functor.Variant as Data.Functor.Variant
import Data.Profunctor.Choice as Data.Profunctor.Choice
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.Tuple as Data.Tuple
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Test.Util as Test.Util
import Type.Proxy as Type.Proxy

data TestF a = TestOp String (String -> a)

derive instance Functor TestF

type TEST r = (test :: TestF | r)

spec :: Test.Spec.Spec Unit
spec = Test.BDD.feature "Diagram generation" do
  Test.BDD.scenario "sequential workflow" do
    Test.Spec.it "contains flowchart TD" do
      let output = testSequentialWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "flowchart TD"
    Test.Spec.it "contains step Double" do
      let output = testSequentialWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Double"
    Test.Spec.it "contains step Add Ten" do
      let output = testSequentialWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Add Ten"
    Test.Spec.it "contains step Show" do
      let output = testSequentialWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Show"
    Test.Spec.it "contains arrow connector" do
      let output = testSequentialWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "-->"

  Test.BDD.scenario "parallel workflow" do
    Test.Spec.it "contains subgraph" do
      let output = testParallelWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "subgraph"
    Test.Spec.it "contains step Double" do
      let output = testParallelWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Double"
    Test.Spec.it "contains step Negate" do
      let output = testParallelWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Negate"
    Test.Spec.it "contains fork" do
      let output = testParallelWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "fork"
    Test.Spec.it "contains join" do
      let output = testParallelWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "join"

  Test.BDD.scenario "choice workflow" do
    Test.Spec.it "contains Decision diamond" do
      let output = testChoiceWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "{Decision}"
    Test.Spec.it "contains Handle Left" do
      let output = testChoiceWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Handle Left"
    Test.Spec.it "contains Handle Right" do
      let output = testChoiceWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Handle Right"
    Test.Spec.it "contains Left label" do
      let output = testChoiceWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Left"
    Test.Spec.it "contains Right label" do
      let output = testChoiceWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Right"
    Test.Spec.it "contains merge" do
      let output = testChoiceWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "merge"

  Test.BDD.scenario "request workflow" do
    Test.Spec.it "contains flowchart TD" do
      let output = testRequestWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "flowchart TD"
    Test.Spec.it "contains labeled effect node" do
      let output = testRequestWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "{{Fetch Data}}"

  Test.BDD.scenario "mapArray workflow" do
    Test.Spec.it "contains For Each subgraph" do
      let output = testMapArrayWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "For Each"
    Test.Spec.it "contains subgraph" do
      let output = testMapArrayWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "subgraph"
    Test.Spec.it "contains inner step name" do
      let output = testMapArrayWorkflow
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Process Item"

  Test.BDD.scenario "single-branch parallel" do
    Test.Spec.it "does not contain fork/join for first" do
      let output = testSingleBranchParFirst
      output `Test.Spec.Assertions.shouldSatisfy` (not <<< Test.Util.containsPattern "fork")
    Test.Spec.it "renders the non-pure branch inline" do
      let output = testSingleBranchParFirst
      output `Test.Spec.Assertions.shouldSatisfy` Test.Util.containsPattern "Double"

  Test.BDD.scenario "pure nodes skipped in sequences" do
    Test.Spec.it "does not render pure placeholder in step-pure-step seq" do
      let output = testPureSkippedInSeq
      (Test.Util.countOccurrences "..." output) `Test.Spec.Assertions.shouldEqual` 0

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

testRequestWorkflow :: String
testRequestWorkflow =
  let
    workflow :: Flow.Types.Workflow () (TEST ()) String String
    workflow = Flow.Types.mkRequest "Fetch Data"
      (\input -> Data.Functor.Variant.inj (Type.Proxy.Proxy :: _ "test") (TestOp input identity))
      (\response -> Flow.Types.Pure identity)
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
