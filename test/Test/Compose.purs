module Test.Compose where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Data.Profunctor as Data.Profunctor
import Data.Profunctor.Choice as Data.Profunctor.Choice
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.Tuple as Data.Tuple
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Util as Test.Util
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

doubleW :: forall i o. Flow.Types.Workflow i o Int Int
doubleW = Flow.Types.Pure (_ * 2)

addTenW :: forall i o. Flow.Types.Workflow i o Int Int
addTenW = Flow.Types.Pure (_ + 10)

showW :: forall i o. Flow.Types.Workflow i o Int String
showW = Flow.Types.Pure show

testLeftIdentity :: forall i o. Flow.Types.Workflow i o Int Int
testLeftIdentity = Control.Category.identity Control.Category.>>> doubleW

testRightIdentity :: forall i o. Flow.Types.Workflow i o Int Int
testRightIdentity = doubleW Control.Category.>>> Control.Category.identity

testAssocLeft :: forall i o. Flow.Types.Workflow i o Int String
testAssocLeft = (doubleW Control.Category.>>> addTenW) Control.Category.>>> showW

testAssocRight :: forall i o. Flow.Types.Workflow i o Int String
testAssocRight = doubleW Control.Category.>>> (addTenW Control.Category.>>> showW)

testDimap :: forall i o. Flow.Types.Workflow i o String String
testDimap = Data.Profunctor.dimap (\_ -> 0) show doubleW

testFirst :: forall i o. Flow.Types.Workflow i o (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple Int String)
testFirst = Data.Profunctor.Strong.first doubleW

testSecond :: forall i o. Flow.Types.Workflow i o (Data.Tuple.Tuple String Int) (Data.Tuple.Tuple String Int)
testSecond = Data.Profunctor.Strong.second doubleW

testChoiceLeft :: forall i o. Flow.Types.Workflow i o (Data.Either.Either Int String) (Data.Either.Either Int String)
testChoiceLeft = Data.Profunctor.Choice.left doubleW

testChoiceRight :: forall i o. Flow.Types.Workflow i o (Data.Either.Either String Int) (Data.Either.Either String Int)
testChoiceRight = Data.Profunctor.Choice.right doubleW

testParallelComp :: forall i o. Flow.Types.Workflow i o (Data.Tuple.Tuple Int Int) (Data.Tuple.Tuple Int Int)
testParallelComp =
  Data.Profunctor.Strong.first doubleW
    Control.Category.>>> Data.Profunctor.Strong.second addTenW

testFanout :: forall i o. Flow.Types.Workflow i o Int (Data.Tuple.Tuple Int Int)
testFanout =
  Data.Profunctor.dimap (\x -> Data.Tuple.Tuple x x) identity
    (Data.Profunctor.Strong.first doubleW Control.Category.>>> Data.Profunctor.Strong.second addTenW)

mergeEither :: Data.Either.Either Int Int -> Int
mergeEither (Data.Either.Left n) = n
mergeEither (Data.Either.Right n) = n

testFanin :: forall i o. Flow.Types.Workflow i o (Data.Either.Either Int Int) Int
testFanin =
  Data.Profunctor.Choice.left doubleW
    Control.Category.>>> Data.Profunctor.Choice.right addTenW
    Control.Category.>>> Flow.Types.Pure mergeEither

testNamedSteps :: forall i o. Flow.Types.Workflow i o Int Int
testNamedSteps =
  Flow.Types.Step "Double" doubleW
    Control.Category.>>> Flow.Types.Step "Add Ten" addTenW

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
