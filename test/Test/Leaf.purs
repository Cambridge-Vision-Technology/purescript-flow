module Test.Leaf where

import Prelude

import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Variant as Data.Variant
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Test.Util as Test.Util
import Type.Proxy as Type.Proxy

type PING r = (ping :: String | r)
type PONG r = (pong :: String | r)

type TICK r = (tick :: Unit | r)
type TOCK r = (tock :: Int | r)

spec :: Test.Spec.Spec Unit
spec = Test.BDD.feature "Leaf Workflow" do
  Test.BDD.scenario "single-event leaf" do
    Test.Spec.it "init emits message and one event produces final result" do
      let
        workflow :: Flow.Types.Workflow (PING ()) (PONG ()) String Int
        workflow = Flow.Types.mkLeaf "echo"
          (\input -> Flow.Types.LeafContinue input (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "pong") ("init:" <> input))))
          ( \_ event ->
              Data.Variant.on (Type.Proxy.Proxy :: _ "ping")
                (\_ -> Flow.Types.LeafDone 42)
                Data.Variant.case_
                event
          )
      case workflow of
        Flow.Types.Leaf _ (Flow.Types.LeafCPS k) ->
          k \initFn stepFn -> do
            case initFn "hello" of
              Flow.Types.LeafContinue state messages ->
                Data.Array.NonEmpty.toArray messages `Test.Spec.Assertions.shouldEqual` [ Data.Variant.inj (Type.Proxy.Proxy :: _ "pong") "init:hello" ]
              Flow.Types.LeafDone _ ->
                Test.Spec.Assertions.fail "Expected LeafContinue"
            case initFn "hello" of
              Flow.Types.LeafContinue state _ -> do
                case stepFn state (Data.Variant.inj (Type.Proxy.Proxy :: _ "ping") "world") of
                  Flow.Types.LeafDone b ->
                    b `Test.Spec.Assertions.shouldEqual` 42
                  Flow.Types.LeafContinue _ _ ->
                    Test.Spec.Assertions.fail "Expected LeafDone"
              Flow.Types.LeafDone _ ->
                Test.Spec.Assertions.fail "Expected LeafContinue"
        _ -> Test.Spec.Assertions.fail "Expected Leaf"

  Test.BDD.scenario "multi-step leaf" do
    Test.Spec.it "processes multiple events before completing" do
      let
        workflow :: Flow.Types.Workflow (TICK ()) (TOCK ()) Unit Int
        workflow = Flow.Types.mkLeaf "counter"
          (\_ -> Flow.Types.LeafContinue 0 (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "tock") 0)))
          ( \count event ->
              Data.Variant.on (Type.Proxy.Proxy :: _ "tick")
                ( \_ ->
                    let
                      next = count + 1
                    in
                      if next >= 3 then
                        Flow.Types.LeafDone next
                      else
                        Flow.Types.LeafContinue next (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "tock") next))
                )
                Data.Variant.case_
                event
          )
      case workflow of
        Flow.Types.Leaf _ (Flow.Types.LeafCPS k) ->
          k \initFn stepFn -> do
            let tick = Data.Variant.inj (Type.Proxy.Proxy :: _ "tick") unit

            case initFn unit of
              Flow.Types.LeafContinue s0 _ -> do
                case stepFn s0 tick of
                  Flow.Types.LeafContinue s1 _ -> do
                    case stepFn s1 tick of
                      Flow.Types.LeafContinue s2 _ -> do
                        case stepFn s2 tick of
                          Flow.Types.LeafDone b ->
                            b `Test.Spec.Assertions.shouldEqual` 3
                          Flow.Types.LeafContinue _ _ ->
                            Test.Spec.Assertions.fail "Expected LeafDone after step 3"
                      Flow.Types.LeafDone _ ->
                        Test.Spec.Assertions.fail "Expected LeafContinue after step 2"
                  Flow.Types.LeafDone _ ->
                    Test.Spec.Assertions.fail "Expected LeafContinue after step 1"
              Flow.Types.LeafDone _ ->
                Test.Spec.Assertions.fail "Expected LeafContinue from init"
        _ -> Test.Spec.Assertions.fail "Expected Leaf"

  Test.BDD.scenario "messages emitted" do
    Test.Spec.it "init and step both produce outgoing messages" do
      let
        workflow :: Flow.Types.Workflow (PING ()) (PONG ()) Unit String
        workflow = Flow.Types.mkLeaf "messenger"
          ( \_ ->
              Flow.Types.LeafContinue (0 :: Int)
                ( Data.Array.NonEmpty.cons' (Data.Variant.inj (Type.Proxy.Proxy :: _ "pong") "started")
                    [ Data.Variant.inj (Type.Proxy.Proxy :: _ "pong") "ready" ]
                )
          )
          ( \_ event ->
              Data.Variant.on (Type.Proxy.Proxy :: _ "ping")
                (\msg -> Flow.Types.LeafDone ("done:" <> msg))
                Data.Variant.case_
                event
          )
      case workflow of
        Flow.Types.Leaf _ (Flow.Types.LeafCPS k) ->
          k \initFn stepFn -> do
            case initFn unit of
              Flow.Types.LeafContinue state messages ->
                Data.Array.NonEmpty.toArray messages `Test.Spec.Assertions.shouldEqual`
                  [ Data.Variant.inj (Type.Proxy.Proxy :: _ "pong") "started"
                  , Data.Variant.inj (Type.Proxy.Proxy :: _ "pong") "ready"
                  ]
              Flow.Types.LeafDone _ ->
                Test.Spec.Assertions.fail "Expected LeafContinue"

            case initFn unit of
              Flow.Types.LeafContinue state _ -> do
                case stepFn state (Data.Variant.inj (Type.Proxy.Proxy :: _ "ping") "test") of
                  Flow.Types.LeafDone b ->
                    b `Test.Spec.Assertions.shouldEqual` "done:test"
                  Flow.Types.LeafContinue _ _ ->
                    Test.Spec.Assertions.fail "Expected LeafDone"
              Flow.Types.LeafDone _ ->
                Test.Spec.Assertions.fail "Expected LeafContinue"
        _ -> Test.Spec.Assertions.fail "Expected Leaf"

  Test.BDD.scenario "immediate completion" do
    Test.Spec.it "leaf init returns LeafDone without events" do
      let
        workflow :: Flow.Types.Workflow (PING ()) (PONG ()) String String
        workflow = Flow.Types.mkLeaf "instant"
          (\input -> Flow.Types.LeafDone ("immediate:" <> input))
          (\_ _ -> Flow.Types.LeafDone "unused")
      case workflow of
        Flow.Types.Leaf _ (Flow.Types.LeafCPS k) ->
          k \initFn _stepFn -> do
            case initFn "test" of
              Flow.Types.LeafDone b ->
                b `Test.Spec.Assertions.shouldEqual` "immediate:test"
              Flow.Types.LeafContinue _ _ ->
                Test.Spec.Assertions.fail "Expected LeafDone"
        _ -> Test.Spec.Assertions.fail "Expected Leaf"

  Test.BDD.scenario "pure workflows still work with new type parameters" do
    Test.Spec.it "Pure (_ * 2) with empty row types" do
      let
        workflow :: Flow.Types.Workflow () () Int Int
        workflow = Flow.Types.Pure (_ * 2)
        result = Test.Util.runPure workflow 5
      result `Test.Spec.Assertions.shouldEqual` 10
