module Flow.Core
  ( module Flow.Types
  , step
  , pure'
  , compose
  , (>>>)
  , parallel
  , (***)
  , fanin
  , (|||)
  , request
  ) where

import Prelude hiding ((>>>))

import Data.Either as Data.Either
import Data.Functor.Variant as Data.Functor.Variant
import Data.Tuple as Data.Tuple
import Flow.Types as Flow.Types

-- | Wrap a workflow with a descriptive name for diagram generation.
-- | Named steps appear as labeled nodes in the workflow diagram.
step :: forall i o a b. String -> Flow.Types.Workflow i o a b -> Flow.Types.Workflow i o a b
step name w = Flow.Types.Step name w

-- | Lift a pure function into a workflow.
-- | Alias for the Pure constructor with a clearer name.
pure' :: forall i o a b. (a -> b) -> Flow.Types.Workflow i o a b
pure' = Flow.Types.Pure

-- | Sequential composition operator.
-- | Composes two workflows where the output of the first feeds into the second.
infixr 1 compose as >>>

compose :: forall i o a b c. Flow.Types.Workflow i o a b -> Flow.Types.Workflow i o b c -> Flow.Types.Workflow i o a c
compose = Flow.Types.mkSeq

-- | Parallel composition operator (split).
-- | Runs two workflows in parallel on tuple input, producing tuple output.
infixr 3 parallel as ***

parallel
  :: forall i o a1 a2 b1 b2
   . Flow.Types.Workflow i o a1 b1
  -> Flow.Types.Workflow i o a2 b2
  -> Flow.Types.Workflow i o (Data.Tuple.Tuple a1 a2) (Data.Tuple.Tuple b1 b2)
parallel = Flow.Types.mkPar

-- | Choice composition operator (fanin).
-- | Given an Either input, runs the appropriate branch workflow.
infixr 2 fanin as |||

fanin
  :: forall i o a b c
   . Flow.Types.Workflow i o a c
  -> Flow.Types.Workflow i o b c
  -> Flow.Types.Workflow i o (Data.Either.Either a b) c
fanin left right = Flow.Types.mkChoice identity left right

-- | Create an effectful request workflow.
-- |
-- | The request is built from the input using `toRequest`, producing a `VariantF o x`.
-- | An effect handler interprets this request and returns the response value `x`.
-- | The continuation `fromResponse` then produces the next workflow step.
-- |
-- | Example:
-- | ```purescript
-- | data HttpF a = Get String (String -> a)
-- | type HTTP r = (http :: HttpF | r)
-- |
-- | fetchUrl :: forall i. Workflow i (HTTP ()) String String
-- | fetchUrl = request
-- |   (\url -> VariantF.inj (Proxy :: _ "http") (Get url identity))
-- |   (\body -> pure' identity)
-- | ```
request
  :: forall i o a b x
   . (a -> Data.Functor.Variant.VariantF o x)
  -> (x -> Flow.Types.Workflow i o x b)
  -> Flow.Types.Workflow i o a b
request = Flow.Types.mkRequest
