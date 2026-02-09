module Test.Effect where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Data.Functor.Variant as Data.Functor.Variant
import Data.Identity as Data.Identity
import Data.Tuple as Data.Tuple
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Util as Test.Util
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Type.Proxy as Type.Proxy

data CounterF a = Increment (Int -> a) | GetCount (Int -> a)

derive instance Functor CounterF

type COUNTER r = (counter :: CounterF | r)

spec :: Test.Spec.Spec Unit
spec = Test.BDD.feature "Effect interpreter" do
  Test.BDD.scenario "pure workflows" do
    Test.Spec.it "applies function to input" do
      let
        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Flow.Types.Pure (_ * 2)
        result = Test.Util.runPure workflow 5
      result `Test.Spec.Assertions.shouldEqual` 10

    Test.Spec.it "preserves identity" do
      let
        workflow :: Flow.Types.Workflow () () String String
        workflow = Flow.Types.Pure identity
        result = Test.Util.runPure workflow "hello"
      result `Test.Spec.Assertions.shouldEqual` "hello"

  Test.BDD.scenario "step execution" do
    Test.Spec.it "step names do not affect execution" do
      let
        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))
        result = Test.Util.runPure workflow 7
      result `Test.Spec.Assertions.shouldEqual` 14

  Test.BDD.scenario "sequential workflows" do
    Test.Spec.it "executes in order" do
      let
        doubleW :: Flow.Types.Workflow () () Int Int
        doubleW = Flow.Types.Pure (_ * 2)

        addTenW :: Flow.Types.Workflow () () Int Int
        addTenW = Flow.Types.Pure (_ + 10)

        workflow :: Flow.Types.Workflow () () Int Int
        workflow = doubleW Control.Category.>>> addTenW
        result = Test.Util.runPure workflow 5
      result `Test.Spec.Assertions.shouldEqual` 20

    Test.Spec.it "composes three workflows" do
      let
        doubleW :: Flow.Types.Workflow () () Int Int
        doubleW = Flow.Types.Pure (_ * 2)

        addTenW :: Flow.Types.Workflow () () Int Int
        addTenW = Flow.Types.Pure (_ + 10)

        negateW :: Flow.Types.Workflow () () Int Int
        negateW = Flow.Types.Pure negate

        workflow :: Flow.Types.Workflow () () Int Int
        workflow = doubleW Control.Category.>>> addTenW Control.Category.>>> negateW
        result = Test.Util.runPure workflow 5
      result `Test.Spec.Assertions.shouldEqual` (-20)

  Test.BDD.scenario "parallel workflows" do
    Test.Spec.it "executes both branches" do
      let
        doubleW :: Flow.Types.Workflow () () Int Int
        doubleW = Flow.Types.Pure (_ * 2)

        negateW :: Flow.Types.Workflow () () Int Int
        negateW = Flow.Types.Pure negate

        workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int Int) (Data.Tuple.Tuple Int Int)
        workflow = Flow.Types.mkPar doubleW negateW
        result = Test.Util.runPure workflow (Data.Tuple.Tuple 5 3)
      result `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple 10 (-3))

    Test.Spec.it "handles different types" do
      let
        showW :: Flow.Types.Workflow () () Int String
        showW = Flow.Types.Pure show

        lengthW :: Flow.Types.Workflow () () String Int
        lengthW = Flow.Types.Pure (\_ -> 5)

        workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple String Int)
        workflow = Flow.Types.mkPar showW lengthW
        result = Test.Util.runPure workflow (Data.Tuple.Tuple 42 "hello")
      result `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple "42" 5)

  Test.BDD.scenario "choice workflows" do
    Test.Spec.it "picks Left branch" do
      let
        handleInt :: Flow.Types.Workflow () () Int String
        handleInt = Flow.Types.Pure (\n -> "int:" <> show n)

        handleString :: Flow.Types.Workflow () () String String
        handleString = Flow.Types.Pure (\s -> "string:" <> s)

        workflow :: Flow.Types.Workflow () () (Data.Either.Either Int String) String
        workflow = Flow.Types.mkChoice identity handleInt handleString
        result = Test.Util.runPure workflow (Data.Either.Left 42)
      result `Test.Spec.Assertions.shouldEqual` "int:42"

    Test.Spec.it "picks Right branch" do
      let
        handleInt :: Flow.Types.Workflow () () Int String
        handleInt = Flow.Types.Pure (\n -> "int:" <> show n)

        handleString :: Flow.Types.Workflow () () String String
        handleString = Flow.Types.Pure (\s -> "string:" <> s)

        workflow :: Flow.Types.Workflow () () (Data.Either.Either Int String) String
        workflow = Flow.Types.mkChoice identity handleInt handleString
        result = Test.Util.runPure workflow (Data.Either.Right "hello")
      result `Test.Spec.Assertions.shouldEqual` "string:hello"

    Test.Spec.it "uses custom splitter" do
      let
        isPositive :: Int -> Data.Either.Either Int Int
        isPositive n = if n >= 0 then Data.Either.Left n else Data.Either.Right n

        handlePositive :: Flow.Types.Workflow () () Int String
        handlePositive = Flow.Types.Pure (\n -> "positive:" <> show n)

        handleNegative :: Flow.Types.Workflow () () Int String
        handleNegative = Flow.Types.Pure (\n -> "negative:" <> show n)

        workflow :: Flow.Types.Workflow () () Int String
        workflow = Flow.Types.mkChoice isPositive handlePositive handleNegative
        positiveResult = Test.Util.runPure workflow 5
        negativeResult = Test.Util.runPure workflow (-3)
      positiveResult `Test.Spec.Assertions.shouldEqual` "positive:5"
      negativeResult `Test.Spec.Assertions.shouldEqual` "negative:-3"

  Test.BDD.scenario "complex workflows" do
    Test.Spec.it "combines sequential, parallel, and choice" do
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
        result = Test.Util.runPure workflow "input"
      result `Test.Spec.Assertions.shouldEqual` "valid:84"

  Test.BDD.scenario "category laws" do
    Test.Spec.it "left identity: id >>> w == w" do
      let
        doubleW :: Flow.Types.Workflow () () Int Int
        doubleW = Flow.Types.Pure (_ * 2)

        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Control.Category.identity Control.Category.>>> doubleW
        result = Test.Util.runPure workflow 5
        expected = Test.Util.runPure doubleW 5
      result `Test.Spec.Assertions.shouldEqual` expected

    Test.Spec.it "right identity: w >>> id == w" do
      let
        doubleW :: Flow.Types.Workflow () () Int Int
        doubleW = Flow.Types.Pure (_ * 2)

        workflow :: Flow.Types.Workflow () () Int Int
        workflow = doubleW Control.Category.>>> Control.Category.identity
        result = Test.Util.runPure workflow 5
        expected = Test.Util.runPure doubleW 5
      result `Test.Spec.Assertions.shouldEqual` expected

  Test.BDD.scenario "named steps" do
    Test.Spec.it "preserves execution semantics" do
      let
        workflow :: Flow.Types.Workflow () () Int Int
        workflow =
          Flow.Types.Step "Double" (Flow.Types.Pure (_ * 2))
            Control.Category.>>> Flow.Types.Step "Add Ten" (Flow.Types.Pure (_ + 10))
            Control.Category.>>> Flow.Types.Step "Negate" (Flow.Types.Pure negate)
        result = Test.Util.runPure workflow 5
      result `Test.Spec.Assertions.shouldEqual` (-20)

  Test.BDD.scenario "request workflows" do
    Test.Spec.it "executes with handler using Identity monad" do
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
      result `Test.Spec.Assertions.shouldEqual` 42

    Test.Spec.it "handles sequential requests" do
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
      result `Test.Spec.Assertions.shouldEqual` 100

    Test.Spec.it "pure workflows work with monadic runner" do
      let
        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Flow.Types.Pure (_ * 2)

        handler :: Flow.Interpret.Effect.EffectHandler Data.Identity.Identity ()
        handler = Data.Functor.Variant.case_
        Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler workflow 5
      result `Test.Spec.Assertions.shouldEqual` 10
