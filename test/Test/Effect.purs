module Test.Effect where

import Prelude

import Control.Category as Control.Category
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Either as Data.Either
import Data.Identity as Data.Identity
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
        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Test.Util.doubleW Control.Category.>>> Test.Util.addTenW
        result = Test.Util.runPure workflow 5
      result `Test.Spec.Assertions.shouldEqual` 20

    Test.Spec.it "composes three workflows" do
      let
        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Test.Util.doubleW Control.Category.>>> Test.Util.addTenW Control.Category.>>> Test.Util.negateW
        result = Test.Util.runPure workflow 5
      result `Test.Spec.Assertions.shouldEqual` (-20)

  Test.BDD.scenario "parallel workflows" do
    Test.Spec.it "executes both branches" do
      let
        workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int Int) (Data.Tuple.Tuple Int Int)
        workflow = Flow.Types.mkPar Test.Util.doubleW Test.Util.negateW
        result = Test.Util.runPure workflow (Data.Tuple.Tuple 5 3)
      result `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple 10 (-3))

    Test.Spec.it "handles different types" do
      let
        lengthW :: Flow.Types.Workflow () () String Int
        lengthW = Flow.Types.Pure (\_ -> 5)

        workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple String Int)
        workflow = Flow.Types.mkPar Test.Util.showW lengthW
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
        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Control.Category.identity Control.Category.>>> Test.Util.doubleW
        result = Test.Util.runPure workflow 5
        expected = Test.Util.runPure Test.Util.doubleW 5
      result `Test.Spec.Assertions.shouldEqual` expected

    Test.Spec.it "right identity: w >>> id == w" do
      let
        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Test.Util.doubleW Control.Category.>>> Control.Category.identity
        result = Test.Util.runPure workflow 5
        expected = Test.Util.runPure Test.Util.doubleW 5
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

  Test.BDD.scenario "mapArray workflows" do
    Test.Spec.it "applies inner workflow to each element" do
      let
        innerW :: Flow.Types.Workflow () () Int Int
        innerW = Flow.Types.Pure (_ * 2)

        workflow :: Flow.Types.Workflow () () (Array Int) (Array Int)
        workflow = Flow.Types.mkMapArray innerW

        result = Test.Util.runPure workflow [ 1, 2, 3 ]
      result `Test.Spec.Assertions.shouldEqual` [ 2, 4, 6 ]

    Test.Spec.it "handles empty array" do
      let
        innerW :: Flow.Types.Workflow () () Int Int
        innerW = Flow.Types.Pure (_ * 2)

        workflow :: Flow.Types.Workflow () () (Array Int) (Array Int)
        workflow = Flow.Types.mkMapArray innerW

        result = Test.Util.runPure workflow []
      result `Test.Spec.Assertions.shouldEqual` []

    Test.Spec.it "works with sequential inner workflow" do
      let
        innerW :: Flow.Types.Workflow () () Int Int
        innerW =
          Flow.Types.Pure (_ * 2)
            Control.Category.>>> Flow.Types.Pure (_ + 1)

        workflow :: Flow.Types.Workflow () () (Array Int) (Array Int)
        workflow = Flow.Types.mkMapArray innerW

        result = Test.Util.runPure workflow [ 5, 10 ]
      result `Test.Spec.Assertions.shouldEqual` [ 11, 21 ]

  Test.BDD.feature "Leaf Execution" do
    Test.BDD.scenario "single-step leaf execution with event handler" do
      Test.Spec.it "sends message via init and processes returned event" do
        let
          singleStepLeaf :: Flow.Types.Workflow (req :: String) (resp :: Int) Unit Int
          singleStepLeaf = Flow.Types.mkLeaf "single"
            (\_ -> Flow.Types.LeafContinue unit (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "resp") 0)))
            ( \_ event ->
                Data.Variant.on (Type.Proxy.Proxy :: _ "req")
                  (\msg -> Flow.Types.LeafDone (Data.String.length msg))
                  Data.Variant.case_
                  event
            )

          handler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (req :: String) (resp :: Int)
          handler = Data.Variant.on (Type.Proxy.Proxy :: _ "resp")
            (\_ -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "req") "response"))
            Data.Variant.case_

          Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler singleStepLeaf unit
        result `Test.Spec.Assertions.shouldEqual` 8

    Test.BDD.scenario "multi-step leaf with state progression" do
      Test.Spec.it "processes three events before completing" do
        let
          counterLeaf :: Flow.Types.Workflow (tick :: Unit) (count :: Int) Unit Int
          counterLeaf = Flow.Types.mkLeaf "counter"
            (\_ -> Flow.Types.LeafContinue 0 (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "count") 0)))
            ( \s event ->
                Data.Variant.on (Type.Proxy.Proxy :: _ "tick")
                  ( \_ ->
                      let
                        newState = s + 1
                      in
                        if newState >= 3 then Flow.Types.LeafDone newState
                        else Flow.Types.LeafContinue newState (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "count") newState))
                  )
                  Data.Variant.case_
                  event
            )

          handler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (tick :: Unit) (count :: Int)
          handler = Data.Variant.on (Type.Proxy.Proxy :: _ "count")
            (\_ -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "tick") unit))
            Data.Variant.case_

          Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler counterLeaf unit
        result `Test.Spec.Assertions.shouldEqual` 3

    Test.BDD.scenario "leaf emits multiple messages per step" do
      Test.Spec.it "processes all returned events from multiple init messages" do
        let
          multiMsgLeaf :: Flow.Types.Workflow (val :: Int) (ask :: String) Unit Int
          multiMsgLeaf = Flow.Types.mkLeaf "multi"
            ( \_ ->
                Flow.Types.LeafContinue 0
                  ( Data.Array.NonEmpty.cons' (Data.Variant.inj (Type.Proxy.Proxy :: _ "ask") "first")
                      [ Data.Variant.inj (Type.Proxy.Proxy :: _ "ask") "second" ]
                  )
            )
            ( \s event ->
                Data.Variant.on (Type.Proxy.Proxy :: _ "val")
                  ( \v ->
                      let
                        newState = s + v
                      in
                        if newState >= 2 then Flow.Types.LeafDone newState
                        else Flow.Types.LeafContinue newState (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "ask") "more"))
                  )
                  Data.Variant.case_
                  event
            )

          handler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (val :: Int) (ask :: String)
          handler = Data.Variant.on (Type.Proxy.Proxy :: _ "ask")
            (\_ -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "val") 1))
            Data.Variant.case_

          Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler multiMsgLeaf unit
        result `Test.Spec.Assertions.shouldEqual` 2

  Test.BDD.feature "Encapsulated Execution" do
    Test.BDD.scenario "stateless encapsulation translates during execution" do
      Test.Spec.it "maps between outer and inner event/message types" do
        let
          innerLeaf :: Flow.Types.Workflow (innerReq :: String) (innerResp :: Int) Unit Int
          innerLeaf = Flow.Types.mkLeaf "inner"
            (\_ -> Flow.Types.LeafContinue unit (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "innerResp") 42)))
            ( \_ event ->
                Data.Variant.on (Type.Proxy.Proxy :: _ "innerReq")
                  (\msg -> Flow.Types.LeafDone (Data.String.length msg))
                  Data.Variant.case_
                  event
            )

          enc :: Flow.Types.Encapsulation (innerReq :: String) (innerResp :: Int) (outerReq :: String) (outerResp :: Int)
          enc = Flow.Types.Encapsulation
            { events: Data.Variant.on (Type.Proxy.Proxy :: _ "outerReq")
                (\s -> Data.Variant.inj (Type.Proxy.Proxy :: _ "innerReq") s)
                Data.Variant.case_
            , messages: Data.Variant.on (Type.Proxy.Proxy :: _ "innerResp")
                (\n -> Data.Variant.inj (Type.Proxy.Proxy :: _ "outerResp") n)
                Data.Variant.case_
            }

          encapsulatedWorkflow :: Flow.Types.Workflow (outerReq :: String) (outerResp :: Int) Unit Int
          encapsulatedWorkflow = Flow.Types.encapsulate "encap" enc innerLeaf

          outerHandler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (outerReq :: String) (outerResp :: Int)
          outerHandler = Data.Variant.on (Type.Proxy.Proxy :: _ "outerResp")
            (\_ -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "outerReq") "hello"))
            Data.Variant.case_

          Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM outerHandler encapsulatedWorkflow unit
        result `Test.Spec.Assertions.shouldEqual` 5

  Test.BDD.feature "Composed Execution" do
    Test.BDD.scenario "sequential leaf workflows execute in order" do
      Test.Spec.it "composes two leaf workflows with >>>" do
        let
          leaf1 :: Flow.Types.Workflow (ev1 :: Int) (msg1 :: String) Unit Int
          leaf1 = Flow.Types.mkLeaf "leaf1"
            (\_ -> Flow.Types.LeafContinue unit (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "msg1") "start")))
            ( \_ event ->
                Data.Variant.on (Type.Proxy.Proxy :: _ "ev1")
                  (\n -> Flow.Types.LeafDone (n * 2))
                  Data.Variant.case_
                  event
            )

          leaf2 :: Flow.Types.Workflow (ev1 :: Int) (msg1 :: String) Int String
          leaf2 = Flow.Types.mkLeaf "leaf2"
            (\n -> Flow.Types.LeafContinue n (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "msg1") "next")))
            ( \s event ->
                Data.Variant.on (Type.Proxy.Proxy :: _ "ev1")
                  (\n -> Flow.Types.LeafDone (show (s + n)))
                  Data.Variant.case_
                  event
            )

          composed :: Flow.Types.Workflow (ev1 :: Int) (msg1 :: String) Unit String
          composed = leaf1 Control.Category.>>> leaf2

          handler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (ev1 :: Int) (msg1 :: String)
          handler = Data.Variant.on (Type.Proxy.Proxy :: _ "msg1")
            (\_ -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "ev1") 3))
            Data.Variant.case_

          Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler composed unit
        result `Test.Spec.Assertions.shouldEqual` "9"

    Test.BDD.scenario "parallel leaf workflows via MonadSchedule" do
      Test.Spec.it "executes both branches of mkPar with pure inner workflows" do
        let
          workflow :: Flow.Types.Workflow () () (Data.Tuple.Tuple Int Int) (Data.Tuple.Tuple Int Int)
          workflow = Flow.Types.mkPar Test.Util.doubleW Test.Util.addTenW

          handler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity () ()
          handler = Data.Variant.case_

          Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler workflow (Data.Tuple.Tuple 5 7)
        result `Test.Spec.Assertions.shouldEqual` (Data.Tuple.Tuple 10 17)

    Test.BDD.scenario "choice with leaf workflows" do
      Test.Spec.it "executes Left branch" do
        let
          leftW :: Flow.Types.Workflow () () Int String
          leftW = Flow.Types.Pure (\n -> "left:" <> show n)

          rightW :: Flow.Types.Workflow () () String String
          rightW = Flow.Types.Pure (\s -> "right:" <> s)

          workflow :: Flow.Types.Workflow () () (Data.Either.Either Int String) String
          workflow = Flow.Types.mkChoice identity leftW rightW

          handler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity () ()
          handler = Data.Variant.case_

          Data.Identity.Identity leftResult = Flow.Interpret.Effect.runWorkflowM handler workflow (Data.Either.Left 42)
        leftResult `Test.Spec.Assertions.shouldEqual` "left:42"

      Test.Spec.it "executes Right branch" do
        let
          leftW :: Flow.Types.Workflow () () Int String
          leftW = Flow.Types.Pure (\n -> "left:" <> show n)

          rightW :: Flow.Types.Workflow () () String String
          rightW = Flow.Types.Pure (\s -> "right:" <> s)

          workflow :: Flow.Types.Workflow () () (Data.Either.Either Int String) String
          workflow = Flow.Types.mkChoice identity leftW rightW

          handler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity () ()
          handler = Data.Variant.case_

          Data.Identity.Identity rightResult = Flow.Interpret.Effect.runWorkflowM handler workflow (Data.Either.Right "hello")
        rightResult `Test.Spec.Assertions.shouldEqual` "right:hello"
