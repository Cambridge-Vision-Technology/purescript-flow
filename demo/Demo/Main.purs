module Demo.Main where

import Prelude

import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Either as Data.Either
import Data.Identity as Data.Identity
import Data.Tuple as Data.Tuple
import Data.Variant as Data.Variant
import Effect as Effect
import Effect.Console as Effect.Console
import Flow.Core as Flow.Core
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types
import Type.Proxy as Type.Proxy

type DomainResponse r = (userFound :: String | r)
type DomainRequest r = (fetchUser :: String | r)

type PlatformResponse r = (httpResult :: String | r)
type PlatformRequest r = (httpGet :: String | r)

fetchUserWorkflow :: Flow.Types.Workflow (DomainResponse ()) (DomainRequest ()) String String
fetchUserWorkflow = Flow.Core.leaf "Fetch User"
  (\userId -> Flow.Types.LeafContinue userId (Data.Array.NonEmpty.singleton (Data.Variant.inj (Type.Proxy.Proxy :: _ "fetchUser") userId)))
  ( \_ event ->
      Data.Variant.on (Type.Proxy.Proxy :: _ "userFound")
        (\userName -> Flow.Types.LeafDone ("User: " <> userName))
        Data.Variant.case_
        event
  )

domainToPlatform
  :: Flow.Types.Encapsulation
       (DomainResponse ())
       (DomainRequest ())
       (PlatformResponse ())
       (PlatformRequest ())
domainToPlatform = Flow.Types.Encapsulation
  { events: Data.Variant.on (Type.Proxy.Proxy :: _ "httpResult")
      (\result -> Data.Variant.inj (Type.Proxy.Proxy :: _ "userFound") result)
      Data.Variant.case_
  , messages: Data.Variant.on (Type.Proxy.Proxy :: _ "fetchUser")
      (\userId -> Data.Variant.inj (Type.Proxy.Proxy :: _ "httpGet") ("/api/users/" <> userId))
      Data.Variant.case_
  }

encapsulatedWorkflow :: Flow.Types.Workflow (PlatformResponse ()) (PlatformRequest ()) String String
encapsulatedWorkflow = Flow.Types.encapsulate "HTTP Transport" domainToPlatform fetchUserWorkflow

platformHandler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (PlatformResponse ()) (PlatformRequest ())
platformHandler = Data.Variant.on (Type.Proxy.Proxy :: _ "httpGet")
  (\url -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "httpResult") ("Alice from " <> url)))
  Data.Variant.case_

type Document =
  { content :: String
  , wordCount :: Int
  }

type ValidationResult =
  { isValid :: Boolean
  , errors :: Array String
  }

type TransformResult =
  { transformed :: String
  , metadata :: String
  }

data ProcessingResult
  = Success { output :: String }
  | Failure { reason :: String }

parseContent :: String -> Document
parseContent input =
  { content: input
  , wordCount: 10
  }

validateDocument :: Document -> ValidationResult
validateDocument doc =
  if doc.wordCount > 0 then
    { isValid: true, errors: [] }
  else
    { isValid: false, errors: [ "Document is empty" ] }

transformDocument :: Document -> TransformResult
transformDocument doc =
  { transformed: "PROCESSED: " <> doc.content
  , metadata: "words=" <> show doc.wordCount
  }

decideOutput
  :: Data.Tuple.Tuple ValidationResult TransformResult
  -> Data.Either.Either String TransformResult
decideOutput (Data.Tuple.Tuple validation transform) =
  if validation.isValid then
    Data.Either.Right transform
  else
    Data.Either.Left (show validation.errors)

formatSuccess :: TransformResult -> ProcessingResult
formatSuccess result = Success { output: result.transformed <> " [" <> result.metadata <> "]" }

formatError :: String -> ProcessingResult
formatError errors = Failure { reason: "Validation failed: " <> errors }

duplicate :: forall a. a -> Data.Tuple.Tuple a a
duplicate x = Data.Tuple.Tuple x x

demoWorkflow :: forall i o. Flow.Types.Workflow i o String ProcessingResult
demoWorkflow =
  Flow.Core.step "Parse Input" (Flow.Core.pure' parseContent)
    Flow.Core.>>> Flow.Core.step "Split" (Flow.Core.pure' duplicate)
    Flow.Core.>>> Flow.Core.step "Process"
      ( Flow.Core.step "Validate" (Flow.Core.pure' validateDocument)
          Flow.Core.*** Flow.Core.step "Transform" (Flow.Core.pure' transformDocument)
      )
    Flow.Core.>>> Flow.Core.step "Decide" (Flow.Core.pure' decideOutput)
    Flow.Core.>>> Flow.Core.step "Output"
      ( Flow.Core.step "Format Error" (Flow.Core.pure' formatError)
          Flow.Core.||| Flow.Core.step "Format Success" (Flow.Core.pure' formatSuccess)
      )

showResult :: ProcessingResult -> String
showResult (Success r) = "Success: " <> r.output
showResult (Failure r) = "Failure: " <> r.reason

alwaysFail :: String -> Data.Either.Either String String
alwaysFail _ = Data.Either.Left "transient error"

timeoutWorkflow
  :: forall i o
   . Flow.Types.Workflow i o String (Data.Either.Either Flow.Types.TimeoutError String)
timeoutWorkflow =
  Flow.Core.timeout (Flow.Types.Milliseconds 5000)
    (Flow.Core.step "Slow Operation" (Flow.Core.pure' identity))

retryWorkflow
  :: forall i o. Flow.Types.Workflow i o String (Flow.Types.RetryResult String String)
retryWorkflow =
  Flow.Core.retry Flow.Core.defaultRetryPolicy
    (Flow.Core.step "Unreliable Call" (Flow.Core.pure' alwaysFail))

showTimeoutResult :: Data.Either.Either Flow.Types.TimeoutError String -> String
showTimeoutResult (Data.Either.Left err) = "Timed out: " <> show err
showTimeoutResult (Data.Either.Right value) = "Completed: " <> value

showRetryResult :: Flow.Types.RetryResult String String -> String
showRetryResult (Flow.Types.RetryResult r) = case r.result of
  Data.Either.Left err ->
    "Failed after " <> show r.attempts <> " attempts: " <> err
  Data.Either.Right value ->
    "Succeeded after " <> show r.attempts <> " attempts: " <> value

main :: Effect.Effect Unit
main = do
  Effect.Console.log "=== Section 1: Event-Driven Reducer (Leaf) ==="
  Effect.Console.log ""
  Effect.Console.log "--- Domain Workflow Diagram ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid fetchUserWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Domain Workflow Execution (with domain handler) ---"
  let
    domainHandler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity (DomainResponse ()) (DomainRequest ())
    domainHandler = Data.Variant.on (Type.Proxy.Proxy :: _ "fetchUser")
      (\userId -> Data.Identity.Identity (Data.Variant.inj (Type.Proxy.Proxy :: _ "userFound") ("Bob (id=" <> userId <> ")")))
      Data.Variant.case_
    Data.Identity.Identity domainResult = Flow.Interpret.Effect.runWorkflowM domainHandler fetchUserWorkflow "user-42"
  Effect.Console.log ("Input: user-42")
  Effect.Console.log ("Output: " <> domainResult)

  Effect.Console.log ""
  Effect.Console.log "=== Section 2: Encapsulation Layers ==="
  Effect.Console.log ""
  Effect.Console.log "--- Encapsulated Workflow Diagram (shows subgraph) ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid encapsulatedWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Encapsulated Workflow Execution (with platform handler) ---"
  let
    Data.Identity.Identity encapResult = Flow.Interpret.Effect.runWorkflowM platformHandler encapsulatedWorkflow "user-99"
  Effect.Console.log ("Input: user-99")
  Effect.Console.log ("Output: " <> encapResult)

  Effect.Console.log ""
  Effect.Console.log "=== Section 3: Pure Composition (>>>, ***, |||) ==="
  Effect.Console.log ""
  Effect.Console.log "--- Workflow Diagram ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid demoWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Execution Result ---"
  let
    input = "Hello world, this is a test document for workflow processing."
    compositionResult = Flow.Interpret.Effect.runWorkflow demoWorkflow input
  Effect.Console.log ("Input: " <> input)
  Effect.Console.log ("Output: " <> showResult compositionResult)

  Effect.Console.log ""
  Effect.Console.log "=== Section 4: Timeout ==="
  Effect.Console.log ""
  Effect.Console.log "--- Workflow Diagram ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid timeoutWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Execution Result ---"
  let
    pureHandler :: Flow.Interpret.Effect.EventHandler Data.Identity.Identity () ()
    pureHandler = Data.Variant.case_
    Data.Identity.Identity timeoutResult =
      Flow.Interpret.Effect.runWorkflowM pureHandler timeoutWorkflow "some input"
  Effect.Console.log ("Output: " <> showTimeoutResult timeoutResult)

  Effect.Console.log ""
  Effect.Console.log "=== Section 5: Retry ==="
  Effect.Console.log ""
  Effect.Console.log "--- Workflow Diagram ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid retryWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Execution Result ---"
  let
    Data.Identity.Identity retryResult =
      Flow.Interpret.Effect.runWorkflowM pureHandler retryWorkflow "some input"
  Effect.Console.log ("Output: " <> showRetryResult retryResult)
