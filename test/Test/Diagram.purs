module Test.Diagram where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Data.Profunctor.Choice as Data.Profunctor.Choice
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.String as Data.String
import Data.Tuple as Data.Tuple
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Types as Flow.Types

-- | Test: Simple Pure workflow produces minimal Mermaid
testPureWorkflow :: String
testPureWorkflow =
  let
    workflow :: Flow.Types.Workflow () () Int Int
    workflow = Flow.Types.Pure (_ * 2)
  in
    Flow.Interpret.Diagram.toMermaid workflow

-- | Test: Single named step produces labeled node
testSingleStep :: String
testSingleStep =
  let
    workflow :: Flow.Types.Workflow () () Int Int
    workflow = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))
  in
    Flow.Interpret.Diagram.toMermaid workflow

-- | Test: Sequential workflow produces connected nodes
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

-- | Test: Parallel workflow uses subgraph
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

-- | Test: Choice workflow uses diamond decision node
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

-- | Test: Complex workflow combining sequential, parallel, and choice
testComplexWorkflow :: String
testComplexWorkflow =
  let
    readW :: Flow.Types.Workflow () () String String
    readW = Flow.Types.Step "Read File" (Flow.Types.Pure identity)

    parseW :: Flow.Types.Workflow () () String String
    parseW = Flow.Types.Step "Parse" (Flow.Types.Pure identity)

    validateW :: Flow.Types.Workflow () () String String
    validateW = Flow.Types.Step "Validate" (Flow.Types.Pure identity)

    transformW :: Flow.Types.Workflow () () String String
    transformW = Flow.Types.Step "Transform" (Flow.Types.Pure identity)

    parallelPart :: Flow.Types.Workflow () () (Data.Tuple.Tuple String String) (Data.Tuple.Tuple String String)
    parallelPart = Flow.Types.mkPar validateW transformW

    workflow :: Flow.Types.Workflow () () String String
    workflow = readW Control.Category.>>> parseW
  in
    Flow.Interpret.Diagram.toMermaid workflow

-- | Verify sequential workflow contains expected elements
verifySequentialContainsElements :: Boolean
verifySequentialContainsElements =
  let
    output = testSequentialWorkflow
  in
    Data.String.contains (Data.String.Pattern "flowchart TD") output
      && Data.String.contains (Data.String.Pattern "Double") output
      && Data.String.contains (Data.String.Pattern "Add Ten") output
      && Data.String.contains (Data.String.Pattern "Show") output
      && Data.String.contains (Data.String.Pattern "-->") output

-- | Verify parallel workflow contains subgraph
verifyParallelContainsSubgraph :: Boolean
verifyParallelContainsSubgraph =
  let
    output = testParallelWorkflow
  in
    Data.String.contains (Data.String.Pattern "subgraph") output
      && Data.String.contains (Data.String.Pattern "Double") output
      && Data.String.contains (Data.String.Pattern "Negate") output
      && Data.String.contains (Data.String.Pattern "fork") output
      && Data.String.contains (Data.String.Pattern "join") output

-- | Verify choice workflow contains diamond node
verifyChoiceContainsDiamond :: Boolean
verifyChoiceContainsDiamond =
  let
    output = testChoiceWorkflow
  in
    Data.String.contains (Data.String.Pattern "{Decision}") output
      && Data.String.contains (Data.String.Pattern "Handle Left") output
      && Data.String.contains (Data.String.Pattern "Handle Right") output
      && Data.String.contains (Data.String.Pattern "Left") output
      && Data.String.contains (Data.String.Pattern "Right") output
      && Data.String.contains (Data.String.Pattern "merge") output

-- | Using Strong instance for parallel (first/second)
testStrongFirst :: String
testStrongFirst =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))

    workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple Int String)
    workflow = Data.Profunctor.Strong.first doubleW
  in
    Flow.Interpret.Diagram.toMermaid workflow

-- | Using Choice instance for branching (left/right)
testChoiceLeft :: String
testChoiceLeft =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))

    workflow :: Flow.Types.Workflow () () (Data.Either.Either Int String) (Data.Either.Either Int String)
    workflow = Data.Profunctor.Choice.left doubleW
  in
    Flow.Interpret.Diagram.toMermaid workflow
