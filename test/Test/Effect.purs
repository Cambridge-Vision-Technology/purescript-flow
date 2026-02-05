module Test.Effect where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Data.Functor.Variant as Data.Functor.Variant
import Data.Identity as Data.Identity
import Data.Tuple as Data.Tuple
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types
import Type.Proxy as Type.Proxy

-- | Test: Pure workflow executes correctly
-- | A pure function lifted into a workflow should apply the function.
testPureWorkflow :: Boolean
testPureWorkflow =
  let
    workflow :: Flow.Types.Workflow () () Int Int
    workflow = Flow.Types.Pure (_ * 2)

    result = Flow.Interpret.Effect.runWorkflow workflow 5
  in
    result == 10

-- | Test: Pure identity workflow
testPureIdentity :: Boolean
testPureIdentity =
  let
    workflow :: Flow.Types.Workflow () () String String
    workflow = Flow.Types.Pure identity

    result = Flow.Interpret.Effect.runWorkflow workflow "hello"
  in
    result == "hello"

-- | Test: Step wrapper doesn't affect execution
-- | Step names are only for diagram generation, not execution.
testStepExecution :: Boolean
testStepExecution =
  let
    workflow :: Flow.Types.Workflow () () Int Int
    workflow = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))

    result = Flow.Interpret.Effect.runWorkflow workflow 7
  in
    result == 14

-- | Test: Sequential workflow executes in order
-- | w1 >>> w2 should apply w1 first, then w2 to the result.
testSequentialWorkflow :: Boolean
testSequentialWorkflow =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Pure (_ * 2)

    addTenW :: Flow.Types.Workflow () () Int Int
    addTenW = Flow.Types.Pure (_ + 10)

    workflow :: Flow.Types.Workflow () () Int Int
    workflow = doubleW Control.Category.>>> addTenW

    result = Flow.Interpret.Effect.runWorkflow workflow 5
  in
    result == 20

-- | Test: Sequential with three workflows
testSequentialThreeWorkflows :: Boolean
testSequentialThreeWorkflows =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Pure (_ * 2)

    addTenW :: Flow.Types.Workflow () () Int Int
    addTenW = Flow.Types.Pure (_ + 10)

    negateW :: Flow.Types.Workflow () () Int Int
    negateW = Flow.Types.Pure negate

    workflow :: Flow.Types.Workflow () () Int Int
    workflow = doubleW Control.Category.>>> addTenW Control.Category.>>> negateW

    result = Flow.Interpret.Effect.runWorkflow workflow 5
  in
    result == -20

-- | Test: Parallel workflow executes both branches
-- | Par should run both workflows on respective tuple components.
testParallelWorkflow :: Boolean
testParallelWorkflow =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Pure (_ * 2)

    negateW :: Flow.Types.Workflow () () Int Int
    negateW = Flow.Types.Pure negate

    workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int Int) (Data.Tuple.Tuple Int Int)
    workflow = Flow.Types.mkPar doubleW negateW

    result = Flow.Interpret.Effect.runWorkflow workflow (Data.Tuple.Tuple 5 3)
  in
    result == Data.Tuple.Tuple 10 (-3)

-- | Test: Parallel with different types
testParallelDifferentTypes :: Boolean
testParallelDifferentTypes =
  let
    showW :: Flow.Types.Workflow () () Int String
    showW = Flow.Types.Pure show

    lengthW :: Flow.Types.Workflow () () String Int
    lengthW = Flow.Types.Pure (\s -> length s)
      where
      length :: String -> Int
      length _ = 5

    workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple String Int)
    workflow = Flow.Types.mkPar showW lengthW

    result = Flow.Interpret.Effect.runWorkflow workflow (Data.Tuple.Tuple 42 "hello")
  in
    result == Data.Tuple.Tuple "42" 5

-- | Test: Choice workflow picks Left branch
testChoiceLeftBranch :: Boolean
testChoiceLeftBranch =
  let
    handleInt :: Flow.Types.Workflow () () Int String
    handleInt = Flow.Types.Pure (\n -> "int:" <> show n)

    handleString :: Flow.Types.Workflow () () String String
    handleString = Flow.Types.Pure (\s -> "string:" <> s)

    splitter :: Data.Either.Either Int String -> Data.Either.Either Int String
    splitter = identity

    workflow :: Flow.Types.Workflow () () (Data.Either.Either Int String) String
    workflow = Flow.Types.mkChoice splitter handleInt handleString

    result = Flow.Interpret.Effect.runWorkflow workflow (Data.Either.Left 42)
  in
    result == "int:42"

-- | Test: Choice workflow picks Right branch
testChoiceRightBranch :: Boolean
testChoiceRightBranch =
  let
    handleInt :: Flow.Types.Workflow () () Int String
    handleInt = Flow.Types.Pure (\n -> "int:" <> show n)

    handleString :: Flow.Types.Workflow () () String String
    handleString = Flow.Types.Pure (\s -> "string:" <> s)

    splitter :: Data.Either.Either Int String -> Data.Either.Either Int String
    splitter = identity

    workflow :: Flow.Types.Workflow () () (Data.Either.Either Int String) String
    workflow = Flow.Types.mkChoice splitter handleInt handleString

    result = Flow.Interpret.Effect.runWorkflow workflow (Data.Either.Right "hello")
  in
    result == "string:hello"

-- | Test: Choice with custom splitter function
testChoiceCustomSplitter :: Boolean
testChoiceCustomSplitter =
  let
    isPositive :: Int -> Data.Either.Either Int Int
    isPositive n = if n >= 0 then Data.Either.Left n else Data.Either.Right n

    handlePositive :: Flow.Types.Workflow () () Int String
    handlePositive = Flow.Types.Pure (\n -> "positive:" <> show n)

    handleNegative :: Flow.Types.Workflow () () Int String
    handleNegative = Flow.Types.Pure (\n -> "negative:" <> show n)

    workflow :: Flow.Types.Workflow () () Int String
    workflow = Flow.Types.mkChoice isPositive handlePositive handleNegative

    positiveResult = Flow.Interpret.Effect.runWorkflow workflow 5
    negativeResult = Flow.Interpret.Effect.runWorkflow workflow (-3)
  in
    positiveResult == "positive:5" && negativeResult == "negative:-3"

-- | Test: Complex workflow combining sequential, parallel, and choice
testComplexWorkflow :: Boolean
testComplexWorkflow =
  let
    parseW :: Flow.Types.Workflow () () String Int
    parseW = Flow.Types.Pure (\_ -> 42)

    dupW :: Flow.Types.Workflow () () Int (Data.Tuple.Tuple Int Int)
    dupW = Flow.Types.Pure (\n -> Data.Tuple.Tuple n n)

    validateW :: Flow.Types.Workflow () () Int Boolean
    validateW = Flow.Types.Pure (\n -> n > 0)

    transformW :: Flow.Types.Workflow () () Int Int
    transformW = Flow.Types.Pure (_ * 2)

    combineW :: Flow.Types.Workflow () () (Data.Tuple.Tuple Boolean Int) String
    combineW = Flow.Types.Pure
      ( \(Data.Tuple.Tuple valid n) ->
          if valid then "valid:" <> show n else "invalid"
      )

    workflow :: Flow.Types.Workflow () () String String
    workflow =
      parseW
        Control.Category.>>> dupW
        Control.Category.>>> Flow.Types.mkPar validateW transformW
        Control.Category.>>> combineW

    result = Flow.Interpret.Effect.runWorkflow workflow "input"
  in
    result == "valid:84"

-- | Test: Category identity law (left)
testCategoryIdentityLeft :: Boolean
testCategoryIdentityLeft =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Pure (_ * 2)

    workflow :: Flow.Types.Workflow () () Int Int
    workflow = Control.Category.identity Control.Category.>>> doubleW

    result = Flow.Interpret.Effect.runWorkflow workflow 5
    expected = Flow.Interpret.Effect.runWorkflow doubleW 5
  in
    result == expected

-- | Test: Category identity law (right)
testCategoryIdentityRight :: Boolean
testCategoryIdentityRight =
  let
    doubleW :: Flow.Types.Workflow () () Int Int
    doubleW = Flow.Types.Pure (_ * 2)

    workflow :: Flow.Types.Workflow () () Int Int
    workflow = doubleW Control.Category.>>> Control.Category.identity

    result = Flow.Interpret.Effect.runWorkflow workflow 5
    expected = Flow.Interpret.Effect.runWorkflow doubleW 5
  in
    result == expected

-- | Test: Named steps preserve execution semantics
testNamedStepsExecution :: Boolean
testNamedStepsExecution =
  let
    workflow :: Flow.Types.Workflow () () Int Int
    workflow =
      Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))
        Control.Category.>>> Flow.Types.Step "Add Ten" (Flow.Types.Pure (_ + 10))
        Control.Category.>>> Flow.Types.Step "Negate" (Flow.Types.Pure negate)

    result = Flow.Interpret.Effect.runWorkflow workflow 5
  in
    result == -20

-- | Effect functor for testing
data CounterF a = Increment (Int -> a) | GetCount (Int -> a)

derive instance Functor CounterF

type COUNTER r = (counter :: CounterF | r)

-- | Test: Request workflow executes with handler (using Identity monad)
testRequestWorkflow :: Boolean
testRequestWorkflow =
  let
    workflow :: Flow.Types.Workflow () (COUNTER ()) Unit Int
    workflow = Flow.Types.mkRequest
      (\_ -> Data.Functor.Variant.inj (Type.Proxy.Proxy :: _ "counter") (GetCount identity))
      (\count -> Flow.Types.Pure (\_ -> count))

    handler :: Flow.Interpret.Effect.EffectHandler Data.Identity.Identity (COUNTER ())
    handler = Data.Functor.Variant.on (Type.Proxy.Proxy :: _ "counter")
      ( \op -> case op of
          GetCount k -> Data.Identity.Identity (k 42)
          Increment k -> Data.Identity.Identity (k 1)
      )
      Data.Functor.Variant.case_

    Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler workflow unit
  in
    result == 42

-- | Test: Sequential requests
testSequentialRequests :: Boolean
testSequentialRequests =
  let
    getW :: Flow.Types.Workflow () (COUNTER ()) Unit Int
    getW = Flow.Types.mkRequest
      (\_ -> Data.Functor.Variant.inj (Type.Proxy.Proxy :: _ "counter") (GetCount identity))
      (\count -> Flow.Types.Pure (\_ -> count))

    incW :: Flow.Types.Workflow () (COUNTER ()) Unit Int
    incW = Flow.Types.mkRequest
      (\_ -> Data.Functor.Variant.inj (Type.Proxy.Proxy :: _ "counter") (Increment identity))
      (\delta -> Flow.Types.Pure (\_ -> delta))

    workflow :: Flow.Types.Workflow () (COUNTER ()) Unit Int
    workflow = incW Control.Category.>>> Flow.Types.Pure (\_ -> unit) Control.Category.>>> getW

    handler :: Flow.Interpret.Effect.EffectHandler Data.Identity.Identity (COUNTER ())
    handler = Data.Functor.Variant.on (Type.Proxy.Proxy :: _ "counter")
      ( \op -> case op of
          GetCount k -> Data.Identity.Identity (k 100)
          Increment k -> Data.Identity.Identity (k 1)
      )
      Data.Functor.Variant.case_

    Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler workflow unit
  in
    result == 100

-- | Test: Pure workflows work with runWorkflowM
testPureWithMonadicRunner :: Boolean
testPureWithMonadicRunner =
  let
    workflow :: Flow.Types.Workflow () () Int Int
    workflow = Flow.Types.Pure (_ * 2)

    handler :: Flow.Interpret.Effect.EffectHandler Data.Identity.Identity ()
    handler = Data.Functor.Variant.case_

    Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler workflow 5
  in
    result == 10

-- | Aggregate all tests for easy verification
allTestsPass :: Boolean
allTestsPass =
  testPureWorkflow
    && testPureIdentity
    && testStepExecution
    && testSequentialWorkflow
    && testSequentialThreeWorkflows
    && testParallelWorkflow
    && testParallelDifferentTypes
    && testChoiceLeftBranch
    && testChoiceRightBranch
    && testChoiceCustomSplitter
    && testComplexWorkflow
    && testCategoryIdentityLeft
    && testCategoryIdentityRight
    && testNamedStepsExecution
    && testRequestWorkflow
    && testSequentialRequests
    && testPureWithMonadicRunner
