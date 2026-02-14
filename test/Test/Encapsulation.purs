module Test.Encapsulation where

import Prelude

import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Variant as Data.Variant
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Type.Proxy as Type.Proxy

type INNER r = (inner :: String | r)
type INNER_MSG r = (innerMsg :: Int | r)

type OUTER r = (outer :: String | r)
type OUTER_MSG r = (outerMsg :: Int | r)

type MIDDLE r = (middle :: String | r)
type MIDDLE_MSG r = (middleMsg :: Int | r)

spec :: Test.Spec.Spec Unit
spec = Test.BDD.feature "Encapsulation" do
  Test.BDD.scenario "stateless" do
    Test.Spec.it "pure event/message translation" do
      let
        innerWorkflow :: Flow.Types.Workflow (INNER ()) (INNER_MSG ()) String Int
        innerWorkflow = Flow.Types.mkLeaf "inner"
          (\input -> Flow.Types.LeafContinue input (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "innerMsg") 0)))
          ( \_ event ->
              Data.Variant.on (Type.Proxy.Proxy :: _ "inner")
                (\_ -> Flow.Types.LeafDone 42)
                Data.Variant.case_
                event
          )

        encapsulation :: Flow.Types.Encapsulation (INNER ()) (INNER_MSG ()) (OUTER ()) (OUTER_MSG ())
        encapsulation = Flow.Types.Encapsulation
          { events: Data.Variant.on (Type.Proxy.Proxy :: _ "outer")
              (\msg -> Data.Variant.inj (Type.Proxy.Proxy :: _ "inner") msg)
              Data.Variant.case_
          , messages: Data.Variant.on (Type.Proxy.Proxy :: _ "innerMsg")
              (\n -> Data.Variant.inj (Type.Proxy.Proxy :: _ "outerMsg") n)
              Data.Variant.case_
          }

        encapsulated :: Flow.Types.Workflow (OUTER ()) (OUTER_MSG ()) String Int
        encapsulated = Flow.Types.encapsulate "wrapper" encapsulation innerWorkflow

      let
        (Flow.Types.Encapsulation fns) = encapsulation
        outerEvent = Data.Variant.inj (Type.Proxy.Proxy :: _ "outer") "hello"
        translatedEvent = fns.events outerEvent
      Data.Variant.on (Type.Proxy.Proxy :: _ "inner")
        (\msg -> msg `Test.Spec.Assertions.shouldEqual` "hello")
        (\_ -> Test.Spec.Assertions.fail "Expected inner variant")
        translatedEvent

      let
        innerMessage = Data.Variant.inj (Type.Proxy.Proxy :: _ "innerMsg") 99
        translatedMessage = fns.messages innerMessage
      Data.Variant.on (Type.Proxy.Proxy :: _ "outerMsg")
        (\n -> n `Test.Spec.Assertions.shouldEqual` 99)
        (\_ -> Test.Spec.Assertions.fail "Expected outerMsg variant")
        translatedMessage

      case encapsulated of
        Flow.Types.Encap label (Flow.Types.EncapCPS k) ->
          k \_enc inner -> do
            label `Test.Spec.Assertions.shouldEqual` "wrapper"
            case inner of
              Flow.Types.Leaf innerLabel _ ->
                innerLabel `Test.Spec.Assertions.shouldEqual` "inner"
              _ -> Test.Spec.Assertions.fail "Expected inner Leaf"
        _ -> Test.Spec.Assertions.fail "Expected Encap"

  Test.BDD.scenario "nested" do
    Test.Spec.it "double-encapsulated workflow composes translations correctly" do
      let
        innerWorkflow :: Flow.Types.Workflow (INNER ()) (INNER_MSG ()) String Int
        innerWorkflow = Flow.Types.mkLeaf "deep-inner"
          (\_ -> Flow.Types.LeafDone 0)
          (\_ _ -> Flow.Types.LeafDone 0)

        innerEncapsulation :: Flow.Types.Encapsulation (INNER ()) (INNER_MSG ()) (MIDDLE ()) (MIDDLE_MSG ())
        innerEncapsulation = Flow.Types.Encapsulation
          { events: Data.Variant.on (Type.Proxy.Proxy :: _ "middle")
              (\msg -> Data.Variant.inj (Type.Proxy.Proxy :: _ "inner") msg)
              Data.Variant.case_
          , messages: Data.Variant.on (Type.Proxy.Proxy :: _ "innerMsg")
              (\n -> Data.Variant.inj (Type.Proxy.Proxy :: _ "middleMsg") n)
              Data.Variant.case_
          }

        middleWorkflow :: Flow.Types.Workflow (MIDDLE ()) (MIDDLE_MSG ()) String Int
        middleWorkflow = Flow.Types.encapsulate "middle-layer" innerEncapsulation innerWorkflow

        outerEncapsulation :: Flow.Types.Encapsulation (MIDDLE ()) (MIDDLE_MSG ()) (OUTER ()) (OUTER_MSG ())
        outerEncapsulation = Flow.Types.Encapsulation
          { events: Data.Variant.on (Type.Proxy.Proxy :: _ "outer")
              (\msg -> Data.Variant.inj (Type.Proxy.Proxy :: _ "middle") msg)
              Data.Variant.case_
          , messages: Data.Variant.on (Type.Proxy.Proxy :: _ "middleMsg")
              (\n -> Data.Variant.inj (Type.Proxy.Proxy :: _ "outerMsg") n)
              Data.Variant.case_
          }

        outerWorkflow :: Flow.Types.Workflow (OUTER ()) (OUTER_MSG ()) String Int
        outerWorkflow = Flow.Types.encapsulate "outer-layer" outerEncapsulation middleWorkflow

      case outerWorkflow of
        Flow.Types.Encap outerLabel (Flow.Types.EncapCPS outerK) ->
          outerK \outerEnc middleW -> do
            outerLabel `Test.Spec.Assertions.shouldEqual` "outer-layer"
            case middleW of
              Flow.Types.Encap middleLabel (Flow.Types.EncapCPS middleK) ->
                middleK \middleEnc innerW -> do
                  middleLabel `Test.Spec.Assertions.shouldEqual` "middle-layer"
                  case innerW of
                    Flow.Types.Leaf innerLabel _ ->
                      innerLabel `Test.Spec.Assertions.shouldEqual` "deep-inner"
                    _ -> Test.Spec.Assertions.fail "Expected inner Leaf at depth 2"
              _ -> Test.Spec.Assertions.fail "Expected Encap at depth 1"
        _ -> Test.Spec.Assertions.fail "Expected Encap at depth 0"

    Test.Spec.it "operator (~>) applies encapsulation" do
      let
        innerWorkflow :: Flow.Types.Workflow (INNER ()) (INNER_MSG ()) String Int
        innerWorkflow = Flow.Types.mkLeaf "operator-test"
          (\_ -> Flow.Types.LeafDone 0)
          (\_ _ -> Flow.Types.LeafDone 0)

        encapsulation :: Flow.Types.Encapsulation (INNER ()) (INNER_MSG ()) (OUTER ()) (OUTER_MSG ())
        encapsulation = Flow.Types.Encapsulation
          { events: Data.Variant.on (Type.Proxy.Proxy :: _ "outer")
              (\msg -> Data.Variant.inj (Type.Proxy.Proxy :: _ "inner") msg)
              Data.Variant.case_
          , messages: Data.Variant.on (Type.Proxy.Proxy :: _ "innerMsg")
              (\n -> Data.Variant.inj (Type.Proxy.Proxy :: _ "outerMsg") n)
              Data.Variant.case_
          }

        encapsulated :: Flow.Types.Workflow (OUTER ()) (OUTER_MSG ()) String Int
        encapsulated = innerWorkflow Flow.Types.~> encapsulation

      case encapsulated of
        Flow.Types.Encap _ (Flow.Types.EncapCPS k) ->
          k \enc inner -> do
            case inner of
              Flow.Types.Leaf label _ ->
                label `Test.Spec.Assertions.shouldEqual` "operator-test"
              _ -> Test.Spec.Assertions.fail "Expected inner Leaf"
        _ -> Test.Spec.Assertions.fail "Expected Encap"
