module Test.EndToEnd where

import Prelude

import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Identity as Data.Identity
import Data.Variant as Data.Variant
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types
import Test.BDD as Test.BDD
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Test.Util as Test.Util
import Type.Proxy as Type.Proxy

type DomainResponse r = (userFound :: String | r)
type DomainRequest r = (fetchUser :: String | r)

type PlatformResponse r = (httpResult :: String | r)
type PlatformRequest r = (httpGet :: String | r)

fetchUserLeaf :: Flow.Types.Workflow (DomainResponse ()) (DomainRequest ()) String String
fetchUserLeaf = Flow.Types.mkLeaf "Fetch User"
  (\userId -> Flow.Types.LeafContinue userId (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "fetchUser") userId)))
  ( \_ event ->
      Data.Variant.on (Type.Proxy.Proxy :: _ "userFound")
        (\userName -> Flow.Types.LeafDone ("User: " <> userName))
        Data.Variant.case_
        event
  )

domainToPlatformEncap
  :: Flow.Types.Encapsulation
       (DomainResponse ())
       (DomainRequest ())
       (PlatformResponse ())
       (PlatformRequest ())
domainToPlatformEncap = Flow.Types.Encapsulation
  { events: Data.Variant.on (Type.Proxy.Proxy :: _ "httpResult")
      (\result -> Data.Variant.inj (Type.Proxy.Proxy :: _ "userFound") result)
      Data.Variant.case_
  , messages: Data.Variant.on (Type.Proxy.Proxy :: _ "fetchUser")
      (\userId -> Data.Variant.inj (Type.Proxy.Proxy :: _ "httpGet") ("/api/users/" <> userId))
      Data.Variant.case_
  }

spec :: Test.Spec.Spec Unit
spec = Test.BDD.feature "End-to-End" do
  Test.BDD.scenario "full application with domain -> platform encapsulation, diagram + execution" do
    Test.Spec.it "generates diagram with encapsulation subgraph and inner leaf" do
      let
        encapsulatedWorkflow :: Flow.Types.Workflow (PlatformResponse ()) (PlatformRequest ()) String String
        encapsulatedWorkflow = Flow.Types.encapsulate "HTTP Transport" domainToPlatformEncap fetchUserLeaf

        diagram = Flow.Interpret.Diagram.toMermaid encapsulatedWorkflow

      Test.Util.containsPattern "subgraph" diagram `Test.Spec.Assertions.shouldEqual` true
      Test.Util.containsPattern "HTTP Transport" diagram `Test.Spec.Assertions.shouldEqual` true
      Test.Util.containsPattern "Fetch User" diagram `Test.Spec.Assertions.shouldEqual` true

    Test.Spec.it "executes encapsulated workflow through platform handler" do
      let
        encapsulatedWorkflow :: Flow.Types.Workflow (PlatformResponse ()) (PlatformRequest ()) String String
        encapsulatedWorkflow = Flow.Types.encapsulate "HTTP Transport" domainToPlatformEncap fetchUserLeaf

        platformHandler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (PlatformResponse ()) (PlatformRequest ())
        platformHandler = Data.Variant.on (Type.Proxy.Proxy :: _ "httpGet")
          (\url -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "httpResult") ("Alice from " <> url)))
          Data.Variant.case_

        Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM platformHandler encapsulatedWorkflow "user-42"

      result `Test.Spec.Assertions.shouldEqual` "User: Alice from /api/users/user-42"

    Test.Spec.it "domain leaf diagram renders without encapsulation" do
      let
        diagram = Flow.Interpret.Diagram.toMermaid fetchUserLeaf

      Test.Util.containsPattern "Fetch User" diagram `Test.Spec.Assertions.shouldEqual` true
      Test.Util.containsPattern "subgraph" diagram `Test.Spec.Assertions.shouldEqual` false

    Test.Spec.it "domain leaf executes with domain handler" do
      let
        domainHandler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (DomainResponse ()) (DomainRequest ())
        domainHandler = Data.Variant.on (Type.Proxy.Proxy :: _ "fetchUser")
          (\userId -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "userFound") ("Bob (id=" <> userId <> ")")))
          Data.Variant.case_

        Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM domainHandler fetchUserLeaf "user-99"

      result `Test.Spec.Assertions.shouldEqual` "User: Bob (id=user-99)"
