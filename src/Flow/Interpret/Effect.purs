module Flow.Interpret.Effect
  ( runWorkflow
  , runWorkflowM
  , EffectHandler
  , runParCPS
  , runChoiceCPS
  , runRequestCPS
  ) where

import Prelude

import Data.Either as Data.Either
import Data.Exists as Data.Exists
import Data.Functor.Variant as Data.Functor.Variant
import Data.Tuple as Data.Tuple
import Flow.Types as Flow.Types
import Partial.Unsafe as Partial.Unsafe

-- | Run a workflow with pure execution.
-- |
-- | This is a pure interpreter that executes workflows without effects.
-- | Workflows containing Request constructors cannot be run with this
-- | interpreter - use runWorkflowM instead.
-- |
-- | Handling:
-- | - `Pure f` -> apply f to input
-- | - `Step _ w` -> recursively run w (step names are just for diagrams)
-- | - `Seq w1 w2` -> run w1, then pass result to w2
-- | - `Par w1 w2` -> run both with respective tuple components
-- | - `Choice splitter left right` -> use splitter, run appropriate branch
-- | - `Request` -> crashes (use runWorkflowM for effectful workflows)
runWorkflow
  :: forall i o a b
   . Flow.Types.Workflow i o a b
  -> a
  -> b
runWorkflow (Flow.Types.Pure f) a = f a

runWorkflow (Flow.Types.Step _ w) a = runWorkflow w a

runWorkflow (Flow.Types.Seq exists) a =
  Data.Exists.runExists (runSeq a) exists

runWorkflow (Flow.Types.Par parCps) a =
  runParCPS parCps a

runWorkflow (Flow.Types.Choice choiceCps) a =
  runChoiceCPS choiceCps a

runWorkflow (Flow.Types.Request _) _ =
  Partial.Unsafe.unsafeCrashWith "runWorkflow: Request constructor requires monadic interpreter (use runWorkflowM)"

-- | Run sequential composition by unwrapping the existential.
runSeq :: forall i o a b x. a -> Flow.Types.SeqF i o a b x -> b
runSeq a (Flow.Types.SeqF w1 w2) =
  let
    intermediate = runWorkflow w1 a
  in
    runWorkflow w2 intermediate

-- | Run parallel composition by unwrapping the CPS encoding.
-- | The CPS provides accessor functions to extract tuple components
-- | and a combiner to merge results.
runParCPS :: forall i o a b. Flow.Types.ParCPS i o a b -> a -> b
runParCPS (Flow.Types.ParCPS k) a =
  k \getFirst getSecond combine w1 w2 ->
    let
      a1 = getFirst a
      a2 = getSecond a
      b1 = runWorkflow w1 a1
      b2 = runWorkflow w2 a2
    in
      combine b1 b2

-- | Run choice composition by unwrapping the CPS encoding.
-- | The CPS provides the splitter function and both branch workflows.
runChoiceCPS :: forall i o a b. Flow.Types.ChoiceCPS i o a b -> a -> b
runChoiceCPS (Flow.Types.ChoiceCPS k) a =
  k \splitter leftW rightW ->
    case splitter a of
      Data.Either.Left x -> runWorkflow leftW x
      Data.Either.Right y -> runWorkflow rightW y

-- | Type alias for effect handlers.
-- |
-- | An effect handler takes a request VariantF and produces a monadic
-- | computation that yields the response value. The response type `x`
-- | is determined by the functor stored in the VariantF.
type EffectHandler m o = forall x. Data.Functor.Variant.VariantF o x -> m x

-- | Run a workflow with monadic effects.
-- |
-- | This interpreter handles all workflow constructors including Request.
-- | The effect handler is called for each Request, interpreting outgoing
-- | requests (VariantF o x) and producing response values (x).
-- |
-- | Handling:
-- | - `Pure f` -> apply f to input, wrapped in pure
-- | - `Step _ w` -> recursively run w (step names are just for diagrams)
-- | - `Seq w1 w2` -> run w1, then pass result to w2 (monadic bind)
-- | - `Par w1 w2` -> run both with respective tuple components
-- | - `Choice splitter left right` -> use splitter, run appropriate branch
-- | - `Request toReq fromResp` -> send request via handler, continue with response
runWorkflowM
  :: forall m i o a b
   . Monad m
  => EffectHandler m o
  -> Flow.Types.Workflow i o a b
  -> a
  -> m b
runWorkflowM _ (Flow.Types.Pure f) a = pure (f a)

runWorkflowM handler (Flow.Types.Step _ w) a = runWorkflowM handler w a

runWorkflowM handler (Flow.Types.Seq exists) a =
  Data.Exists.runExists (runSeqM handler a) exists

runWorkflowM handler (Flow.Types.Par parCps) a =
  runParCPSM handler parCps a

runWorkflowM handler (Flow.Types.Choice choiceCps) a =
  runChoiceCPSM handler choiceCps a

runWorkflowM handler (Flow.Types.Request requestCps) a =
  runRequestCPS handler requestCps a

-- | Run sequential composition monadically.
runSeqM
  :: forall m i o a b x
   . Monad m
  => EffectHandler m o
  -> a
  -> Flow.Types.SeqF i o a b x
  -> m b
runSeqM handler a (Flow.Types.SeqF w1 w2) = do
  intermediate <- runWorkflowM handler w1 a
  runWorkflowM handler w2 intermediate

-- | Run parallel composition monadically.
-- |
-- | NOTE: This runs branches sequentially. For true parallel execution,
-- | the monad must support parallelism (e.g., Aff with parallel combinators).
runParCPSM
  :: forall m i o a b
   . Monad m
  => EffectHandler m o
  -> Flow.Types.ParCPS i o a b
  -> a
  -> m b
runParCPSM handler (Flow.Types.ParCPS k) a =
  k \getFirst getSecond combine w1 w2 -> do
    let a1 = getFirst a
    let a2 = getSecond a
    b1 <- runWorkflowM handler w1 a1
    b2 <- runWorkflowM handler w2 a2
    pure (combine b1 b2)

-- | Run choice composition monadically.
runChoiceCPSM
  :: forall m i o a b
   . Monad m
  => EffectHandler m o
  -> Flow.Types.ChoiceCPS i o a b
  -> a
  -> m b
runChoiceCPSM handler (Flow.Types.ChoiceCPS k) a =
  k \splitter leftW rightW ->
    case splitter a of
      Data.Either.Left x -> runWorkflowM handler leftW x
      Data.Either.Right y -> runWorkflowM handler rightW y

-- | Run request by sending to handler and continuing with response.
runRequestCPS
  :: forall m i o a b
   . Monad m
  => EffectHandler m o
  -> Flow.Types.RequestCPS i o a b
  -> a
  -> m b
runRequestCPS handler (Flow.Types.RequestCPS k) a =
  k \toRequest fromResponse -> do
    let request = toRequest a
    response <- handler request
    runWorkflowM handler (fromResponse response) response
