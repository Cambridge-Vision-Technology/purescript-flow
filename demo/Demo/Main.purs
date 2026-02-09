module Demo.Main where

import Prelude

import Data.Either as Data.Either
import Data.Tuple as Data.Tuple
import Effect as Effect
import Effect.Console as Effect.Console
import Flow as Flow
import Flow.Core as Flow.Core
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Data.Functor.Variant as Data.Functor.Variant
import Data.Identity as Data.Identity
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types

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
  , wordCount: countWords input
  }
  where
  countWords :: String -> Int
  countWords s = 10

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
  Effect.Console.log "=== Composition Demo ==="
  Effect.Console.log ""
  Effect.Console.log "--- Workflow Diagram ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid demoWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Execution Result ---"
  let input = "Hello world, this is a test document for workflow processing."
  let
    handler :: Flow.Interpret.Effect.EffectHandler Data.Identity.Identity ()
    handler = Data.Functor.Variant.case_
    Data.Identity.Identity result = Flow.Interpret.Effect.runWorkflowM handler demoWorkflow input
  Effect.Console.log ("Input: " <> input)
  Effect.Console.log ("Output: " <> showResult result)

  Effect.Console.log ""
  Effect.Console.log "=== Timeout Demo ==="
  Effect.Console.log ""
  Effect.Console.log "--- Workflow Diagram ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid timeoutWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Execution Result ---"
  let
    Data.Identity.Identity timeoutResult =
      Flow.Interpret.Effect.runWorkflowM handler timeoutWorkflow "some input"
  Effect.Console.log ("Output: " <> showTimeoutResult timeoutResult)

  Effect.Console.log ""
  Effect.Console.log "=== Retry Demo ==="
  Effect.Console.log ""
  Effect.Console.log "--- Workflow Diagram ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid retryWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Execution Result ---"
  let
    Data.Identity.Identity retryResult =
      Flow.Interpret.Effect.runWorkflowM handler retryWorkflow "some input"
  Effect.Console.log ("Output: " <> showRetryResult retryResult)
