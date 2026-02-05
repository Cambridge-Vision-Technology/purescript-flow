module Demo.Main where

import Prelude

import Data.Either as Data.Either
import Data.Tuple as Data.Tuple
import Effect as Effect
import Effect.Console as Effect.Console
import Flow as Flow
import Flow.Core as Flow.Core
import Flow.Interpret.Diagram as Flow.Interpret.Diagram
import Flow.Interpret.Effect as Flow.Interpret.Effect
import Flow.Types as Flow.Types

-- | Document data structure (simulating parsed file content)
type Document =
  { content :: String
  , wordCount :: Int
  }

-- | Result of validation
type ValidationResult =
  { isValid :: Boolean
  , errors :: Array String
  }

-- | Result of transformation
type TransformResult =
  { transformed :: String
  , metadata :: String
  }

-- | Final processing result
data ProcessingResult
  = Success { output :: String }
  | Failure { reason :: String }

-- | Parse raw input into a Document
parseContent :: String -> Document
parseContent input =
  { content: input
  , wordCount: countWords input
  }
  where
  countWords :: String -> Int
  countWords s = 10

-- | Validate the document
validateDocument :: Document -> ValidationResult
validateDocument doc =
  if doc.wordCount > 0 then
    { isValid: true, errors: [] }
  else
    { isValid: false, errors: [ "Document is empty" ] }

-- | Transform the document
transformDocument :: Document -> TransformResult
transformDocument doc =
  { transformed: "PROCESSED: " <> doc.content
  , metadata: "words=" <> show doc.wordCount
  }

-- | Decide the output path based on validation
decideOutput
  :: Data.Tuple.Tuple ValidationResult TransformResult
  -> Data.Either.Either String TransformResult
decideOutput (Data.Tuple.Tuple validation transform) =
  if validation.isValid then
    Data.Either.Right transform
  else
    Data.Either.Left (show validation.errors)

-- | Format successful output
formatSuccess :: TransformResult -> ProcessingResult
formatSuccess result = Success { output: result.transformed <> " [" <> result.metadata <> "]" }

-- | Format error output
formatError :: String -> ProcessingResult
formatError errors = Failure { reason: "Validation failed: " <> errors }

-- | Duplicate input for parallel processing
duplicate :: forall a. a -> Data.Tuple.Tuple a a
duplicate x = Data.Tuple.Tuple x x

-- | The main processing workflow demonstrating all composition patterns.
-- |
-- | Workflow structure:
-- | 1. Parse input (pure transformation)
-- | 2. Duplicate the document for parallel processing
-- | 3. In parallel: validate AND transform
-- | 4. Decide based on validation result
-- | 5. If valid: format success output, else: format error
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

-- | Render the processing result for display
showResult :: ProcessingResult -> String
showResult (Success r) = "Success: " <> r.output
showResult (Failure r) = "Failure: " <> r.reason

main :: Effect.Effect Unit
main = do
  Effect.Console.log "--- Workflow Diagram ---"
  Effect.Console.log (Flow.Interpret.Diagram.toMermaid demoWorkflow)
  Effect.Console.log ""
  Effect.Console.log "--- Execution Result ---"
  let input = "Hello world, this is a test document for workflow processing."
  let result = Flow.Interpret.Effect.runWorkflow demoWorkflow input
  Effect.Console.log ("Input: " <> input)
  Effect.Console.log ("Output: " <> showResult result)
