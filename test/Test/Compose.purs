module Test.Compose where

import Prelude

import Control.Category as Control.Category
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Either as Data.Either
import Data.Identity as Data.Identity
import Data.Profunctor as Data.Profunctor
import Data.Profunctor.Choice as Data.Profunctor.Choice
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.String as Data.String
import Data.Tuple as Data.Tuple
import Data.Variant as Data.Variant
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Util as Test.Util
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Type.Proxy as Type.Proxy

testLeftIdentity :: forall i o. Flow.Types.Workflow i o Int Int
testLeftIdentity = Control.Category.identity Control.Category.>>> Test.Util.doubleW

testRightIdentity :: forall i o. Flow.Types.Workflow i o Int Int
testRightIdentity = Test.Util.doubleW Control.Category.>>> Control.Category.identity

testAssocLeft :: forall i o. Flow.Types.Workflow i o Int String
testAssocLeft = (Test.Util.doubleW Control.Category.>>> Test.Util.addTenW) Control.Category.>>> Test.Util.showW

testAssocRight :: forall i o. Flow.Types.Workflow i o Int String
testAssocRight = Test.Util.doubleW Control.Category.>>> (Test.Util.addTenW Control.Category.>>> Test.Util.showW)

testDimap :: forall i o. Flow.Types.Workflow i o String String
testDimap = Data.Profunctor.dimap (\_ -> 0) show Test.Util.doubleW

testFirst :: forall i o. Flow.Types.Workflow i o (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple Int String)
testFirst = Data.Profunctor.Strong.first Test.Util.doubleW

testSecond :: forall i o. Flow.Types.Workflow i o (Data.Tuple.Tuple String Int) (Data.Tuple.Tuple String Int)
testSecond = Data.Profunctor.Strong.second Test.Util.doubleW

testChoiceLeft :: forall i o. Flow.Types.Workflow i o (Data.Either.Either Int String) (Data.Either.Either Int String)
testChoiceLeft = Data.Profunctor.Choice.left Test.Util.doubleW

testChoiceRight :: forall i o. Flow.Types.Workflow i o (Data.Either.Either String Int) (Data.Either.Either String Int)
testChoiceRight = Data.Profunctor.Choice.right Test.Util.doubleW

testParallelComp :: forall i o. Flow.Types.Workflow i o (Data.Tuple.Tuple Int Int) (Data.Tuple.Tuple Int Int)
testParallelComp =
  Data.Profunctor.Strong.first Test.Util.doubleW
    Control.Category.>>> Data.Profunctor.Strong.second Test.Util.addTenW

testFanout :: forall i o. Flow.Types.Workflow i o Int (Data.Tuple.Tuple Int Int)
testFanout =
  Data.Profunctor.dimap (\x -> Data.Tuple.Tuple x x) identity
    (Data.Profunctor.Strong.first Test.Util.doubleW Control.Category.>>> Data.Profunctor.Strong.second Test.Util.addTenW)

mergeEither :: Data.Either.Either Int Int -> Int
mergeEither (Data.Either.Left n) = n
mergeEither (Data.Either.Right n) = n

testFanin :: forall i o. Flow.Types.Workflow i o (Data.Either.Either Int Int) Int
testFanin =
  Data.Profunctor.Choice.left Test.Util.doubleW
    Control.Category.>>> Data.Profunctor.Choice.right Test.Util.addTenW
    Control.Category.>>> Flow.Types.Pure mergeEither

testNamedSteps :: forall i o. Flow.Types.Workflow i o Int Int
testNamedSteps =
  Flow.Types.Step "Double" Test.Util.doubleW
    Control.Category.>>> Flow.Types.Step "Add Ten" Test.Util.addTenW

simpleLeafW :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) Int Int
simpleLeafW = Flow.Types.mkLeaf "test"
  (\n -> Flow.Types.LeafContinue n (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "msg") unit)))
  ( \s event -> Data.Variant.on (Type.Proxy.Proxy :: _ "ev")
      (\_ -> Flow.Types.LeafDone (s * 2))
      Data.Variant.case_
      event
  )

testHandler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (ev :: Unit) (msg :: Unit)
testHandler = Data.Variant.on (Type.Proxy.Proxy :: _ "msg")
  (\_ -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "ev") unit))
  Data.Variant.case_

runWithHandler :: forall a b. Flow.Types.Workflow (ev :: Unit) (msg :: Unit) a b -> a -> b
runWithHandler w a =
  let
    Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM testHandler w a
  in
    result

spec :: Test.Spec.Spec Unit
spec = Test.BDD.feature "Composition primitives" do
  Test.BDD.scenario "category laws" do
    Test.Spec.it "left identity" do
      Test.Util.runPure testLeftIdentity 5 `Test.Spec.Assertions.shouldEqual` 10

    Test.Spec.it "right identity" do
      Test.Util.runPure testRightIdentity 5 `Test.Spec.Assertions.shouldEqual` 10

    Test.Spec.it "associativity (left)" do
      Test.Util.runPure testAssocLeft 5 `Test.Spec.Assertions.shouldEqual` "20"

    Test.Spec.it "associativity (right)" do
      Test.Util.runPure testAssocRight 5 `Test.Spec.Assertions.shouldEqual` "20"

  Test.BDD.scenario "profunctor" do
    Test.Spec.it "dimap" do
      Test.Util.runPure testDimap "x" `Test.Spec.Assertions.shouldEqual` "0"

  Test.BDD.scenario "strong" do
    Test.Spec.it "first" do
      Test.Util.runPure testFirst (Data.Tuple.Tuple 5 "hi") `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple 10 "hi")

    Test.Spec.it "second" do
      Test.Util.runPure testSecond (Data.Tuple.Tuple "hi" 5) `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple "hi" 10)

  Test.BDD.scenario "choice" do
    Test.Spec.it "left" do
      Test.Util.runPure testChoiceLeft (Data.Either.Left 5) `Test.Spec.Assertions.shouldEqual` (Data.Either.Left 10)

    Test.Spec.it "right" do
      Test.Util.runPure testChoiceRight (Data.Either.Right 5) `Test.Spec.Assertions.shouldEqual` (Data.Either.Right 10)

  Test.BDD.scenario "parallel composition" do
    Test.Spec.it "first >>> second" do
      Test.Util.runPure testParallelComp (Data.Tuple.Tuple 5 3) `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple 10 13)

  Test.BDD.scenario "fanout pattern" do
    Test.Spec.it "fanout" do
      Test.Util.runPure testFanout 5 `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple 10 15)

  Test.BDD.scenario "fanin pattern" do
    Test.Spec.it "fanin Left" do
      Test.Util.runPure testFanin (Data.Either.Left 5) `Test.Spec.Assertions.shouldEqual` 10

    Test.Spec.it "fanin Right" do
      Test.Util.runPure testFanin (Data.Either.Right 5)
        `Test.Spec.Assertions.shouldEqual` 15

  Test.BDD.scenario "named steps" do
    Test.Spec.it "named steps" do
      Test.Util.runPure testNamedSteps 5 `Test.Spec.Assertions.shouldEqual` 20

  Test.BDD.feature "Algebraic Laws with Leaf workflows" do
    Test.BDD.scenario "identity laws" do
      Test.Spec.it "left identity: id >>> leaf == leaf" do
        let
          workflow = Control.Category.identity Control.Category.>>> simpleLeafW
          result = runWithHandler workflow 5
        result `Test.Spec.Assertions.shouldEqual` 10

      Test.Spec.it "right identity: leaf >>> id == leaf" do
        let
          workflow = simpleLeafW Control.Category.>>> Control.Category.identity
          result = runWithHandler workflow 5
        result `Test.Spec.Assertions.shouldEqual` 10

    Test.BDD.scenario "associativity" do
      Test.Spec.it "(leaf >>> pure) >>> pure == leaf >>> (pure >>> pure)" do
        let
          addOneW :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) Int Int
          addOneW = Flow.Types.Pure (_ + 1)

          showW' :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) Int String
          showW' = Flow.Types.Pure show

          leftAssoc = (simpleLeafW Control.Category.>>> addOneW) Control.Category.>>> showW'
          rightAssoc = simpleLeafW Control.Category.>>> (addOneW Control.Category.>>> showW')
          leftResult = runWithHandler leftAssoc 5
          rightResult = runWithHandler rightAssoc 5
        leftResult `Test.Spec.Assertions.shouldEqual` "11"
        rightResult `Test.Spec.Assertions.shouldEqual` "11"
        leftResult `Test.Spec.Assertions.shouldEqual` rightResult

    Test.BDD.scenario "profunctor (dimap)" do
      Test.Spec.it "dimap pre-processes input and post-processes output" do
        let
          dimappedW :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) String String
          dimappedW = Data.Profunctor.dimap Data.String.length show simpleLeafW
          result = runWithHandler dimappedW "hello"
        result `Test.Spec.Assertions.shouldEqual` "10"

    Test.BDD.scenario "strong (first/second) with leaf workflows" do
      Test.Spec.it "first applies leaf to first element of tuple" do
        let
          workflow :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple Int String)
          workflow = Data.Profunctor.Strong.first simpleLeafW
          result = runWithHandler workflow (Data.Tuple.Tuple 5 "hi")
        result `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple 10 "hi")

      Test.Spec.it "second applies leaf to second element of tuple" do
        let
          workflow :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) (Data.Tuple.Tuple String Int) (Data.Tuple.Tuple String Int)
          workflow = Data.Profunctor.Strong.second simpleLeafW
          result = runWithHandler workflow (Data.Tuple.Tuple "hi" 5)
        result `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple "hi" 10)

    Test.BDD.scenario "choice (left/right) with leaf workflows" do
      Test.Spec.it "left applies leaf to Left value" do
        let
          workflow :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) (Data.Either.Either Int String) (Data.Either.Either Int String)
          workflow = Data.Profunctor.Choice.left simpleLeafW
          result = runWithHandler workflow (Data.Either.Left 5)
        result `Test.Spec.Assertions.shouldEqual` (Data.Either.Left 10)

      Test.Spec.it "left passes through Right value" do
        let
          workflow :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) (Data.Either.Either Int String) (Data.Either.Either Int String)
          workflow = Data.Profunctor.Choice.left simpleLeafW
          result = runWithHandler workflow (Data.Either.Right "pass")
        result `Test.Spec.Assertions.shouldEqual` (Data.Either.Right "pass")

      Test.Spec.it "right applies leaf to Right value" do
        let
          workflow :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) (Data.Either.Either String Int) (Data.Either.Either String Int)
          workflow = Data.Profunctor.Choice.right simpleLeafW
          result = runWithHandler workflow (Data.Either.Right 5)
        result `Test.Spec.Assertions.shouldEqual` (Data.Either.Right 10)

      Test.Spec.it "right passes through Left value" do
        let
          workflow :: Flow.Types.Workflow (ev :: Unit) (msg :: Unit) (Data.Either.Either String Int) (Data.Either.Either String Int)
          workflow = Data.Profunctor.Choice.right simpleLeafW
          result = runWithHandler workflow (Data.Either.Left "pass")
        result `Test.Spec.Assertions.shouldEqual` (Data.Either.Left "pass")
