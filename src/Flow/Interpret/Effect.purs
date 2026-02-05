module Flow.Interpret.Effect
  ( runWorkflow
  , runParCPS
  , runChoiceCPS
  ) where

import Prelude

import Data.Either as Data.Either
import Data.Exists as Data.Exists
import Data.Tuple as Data.Tuple
import Flow.Types as Flow.Types

-- | Run a workflow with pure execution.
-- |
-- | This is a pure interpreter that executes workflows without effects.
-- | Since the Workflow type currently has no Request constructor,
-- | all execution is pure.
-- |
-- | Handling:
-- | - `Pure f` -> apply f to input
-- | - `Step _ w` -> recursively run w (step names are just for diagrams)
-- | - `Seq w1 w2` -> run w1, then pass result to w2
-- | - `Par w1 w2` -> run both with respective tuple components
-- | - `Choice splitter left right` -> use splitter, run appropriate branch
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
