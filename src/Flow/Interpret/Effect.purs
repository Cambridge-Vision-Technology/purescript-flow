module Flow.Interpret.Effect
  ( runWorkflowM
  , runWorkflow
  , EventHandler
  , calculateBackoff
  ) where

import Prelude

import Data.Array as Data.Array
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Either as Data.Either
import Data.Exists as Data.Exists
import Data.Identity as Data.Identity
import Data.Int as Data.Int
import Data.Maybe as Data.Maybe
import Data.Traversable as Data.Traversable
import Data.Tuple as Data.Tuple
import Data.Variant as Data.Variant
import Flow.Class.Schedule as Flow.Class.Schedule
import Flow.Types as Flow.Types

type EventHandler m i o = Data.Variant.Variant o -> m (Data.Variant.Variant i)

runWorkflow :: forall a b. Flow.Types.Workflow () () a b -> a -> b
runWorkflow w a =
  let
    handler :: EventHandler Data.Identity.Identity () ()
    handler = Data.Variant.case_
    Data.Identity.Identity result = runWorkflowM handler w a
  in
    result

runWorkflowM
  :: forall m i o a b
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
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

runWorkflowM handler (Flow.Types.Leaf _ leafCps) a =
  runLeafCPS handler leafCps a

runWorkflowM handler (Flow.Types.Encap _ encapCps) a =
  runEncapCPS handler encapCps a

runWorkflowM handler (Flow.Types.MapArray mapArrayCps) a =
  runMapArrayCPSM handler mapArrayCps a

runWorkflowM handler (Flow.Types.Timeout timeoutCps) a =
  runTimeoutCPSM handler timeoutCps a

runWorkflowM handler (Flow.Types.Retry retryCps) a =
  runRetryCPSM handler retryCps a

runSeqM
  :: forall m i o a b x
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
  -> a
  -> Flow.Types.SeqF i o a b x
  -> m b
runSeqM handler a (Flow.Types.SeqF w1 w2) = do
  intermediate <- runWorkflowM handler w1 a
  runWorkflowM handler w2 intermediate

runParCPSM
  :: forall m i o a b
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
  -> Flow.Types.ParCPS i o a b
  -> a
  -> m b
runParCPSM handler (Flow.Types.ParCPS k) a =
  k \getFirst getSecond combine w1 w2 -> do
    let a1 = getFirst a
    let a2 = getSecond a
    Data.Tuple.Tuple b1 b2 <- Flow.Class.Schedule.parallel
      (runWorkflowM handler w1 a1)
      (runWorkflowM handler w2 a2)
    pure (combine b1 b2)

runChoiceCPSM
  :: forall m i o a b
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
  -> Flow.Types.ChoiceCPS i o a b
  -> a
  -> m b
runChoiceCPSM handler (Flow.Types.ChoiceCPS k) a =
  k \splitter leftW rightW ->
    case splitter a of
      Data.Either.Left x -> runWorkflowM handler leftW x
      Data.Either.Right y -> runWorkflowM handler rightW y

runLeafCPS
  :: forall m i o a b
   . Monad m
  => EventHandler m i o
  -> Flow.Types.LeafCPS i o a b
  -> a
  -> m b
runLeafCPS handler (Flow.Types.LeafCPS k) a =
  k \initFn stepFn ->
    case initFn a of
      Flow.Types.LeafDone b ->
        pure b
      Flow.Types.LeafContinue state messages ->
        processLeafMessages handler stepFn state (Data.Array.NonEmpty.toArray messages)

processLeafMessages
  :: forall m i o s b
   . Monad m
  => EventHandler m i o
  -> (s -> Data.Variant.Variant i -> Flow.Types.LeafStep o s b)
  -> s
  -> Array (Data.Variant.Variant o)
  -> m b
processLeafMessages handler stepFn state messages = do
  events <- Data.Traversable.traverse handler messages
  processLeafEvents handler stepFn state events

processLeafEvents
  :: forall m i o s b
   . Monad m
  => EventHandler m i o
  -> (s -> Data.Variant.Variant i -> Flow.Types.LeafStep o s b)
  -> s
  -> Array (Data.Variant.Variant i)
  -> m b
processLeafEvents handler stepFn state events = case Data.Array.uncons events of
  Data.Maybe.Nothing ->
    processLeafMessages handler stepFn state []
  Data.Maybe.Just { head: event, tail: rest } ->
    case stepFn state event of
      Flow.Types.LeafDone b ->
        pure b
      Flow.Types.LeafContinue newState messages ->
        let
          newMessages = Data.Array.NonEmpty.toArray messages
        in
          do
            moreEvents <- Data.Traversable.traverse handler newMessages
            processLeafEvents handler stepFn newState (rest <> moreEvents)

runEncapCPS
  :: forall m i o a b
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
  -> Flow.Types.EncapCPS i o a b
  -> a
  -> m b
runEncapCPS outerHandler (Flow.Types.EncapCPS k) a =
  k \enc innerWorkflow ->
    let
      (Flow.Types.Encapsulation fns) = enc

      innerHandler :: EventHandler m _ _
      innerHandler msg = do
        let outerMsg = fns.messages msg
        outerEvent <- outerHandler outerMsg
        pure (fns.events outerEvent)
    in
      runWorkflowM innerHandler innerWorkflow a

runMapArrayCPSM
  :: forall m i o a b
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
  -> Flow.Types.MapArrayCPS i o a b
  -> a
  -> m b
runMapArrayCPSM handler (Flow.Types.MapArrayCPS k) a =
  k \inner split combine -> do
    let elements = split a
    results <- Data.Traversable.traverse (\x -> runWorkflowM handler inner x) elements
    pure (combine results)

runTimeoutCPSM
  :: forall m i o a b
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
  -> Flow.Types.TimeoutCPS i o a b
  -> a
  -> m b
runTimeoutCPSM handler (Flow.Types.TimeoutCPS k) a =
  k \duration inner transform -> do
    raceResult <- Flow.Class.Schedule.race
      (Flow.Class.Schedule.delay duration $> Data.Either.Left (Flow.Types.TimedOut duration))
      (Data.Either.Right <$> runWorkflowM handler inner a)
    let
      result = case raceResult of
        Data.Either.Left timeoutErr -> timeoutErr
        Data.Either.Right workflowResult -> workflowResult
    pure (transform result)

runRetryCPSM
  :: forall m i o a b
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
  -> Flow.Types.RetryCPS i o a b
  -> a
  -> m b
runRetryCPSM handler (Flow.Types.RetryCPS k) a =
  k \policy inner transform -> do
    result <- retryLoop handler policy inner a 1
    pure (transform result)

retryLoop
  :: forall m i o a e innerB
   . Monad m
  => Flow.Class.Schedule.MonadSchedule m
  => EventHandler m i o
  -> Flow.Types.RetryPolicy
  -> Flow.Types.Workflow i o a (Data.Either.Either e innerB)
  -> a
  -> Int
  -> m (Flow.Types.RetryResult e innerB)
retryLoop handler policy inner a attempt = do
  let (Flow.Types.RetryPolicy p) = policy
  result <- runWorkflowM handler inner a
  case result of
    Data.Either.Right success ->
      pure (Flow.Types.RetryResult { attempts: attempt, result: Data.Either.Right success })
    Data.Either.Left err
      | attempt >= p.maxAttempts ->
          pure (Flow.Types.RetryResult { attempts: attempt, result: Data.Either.Left err })
      | otherwise -> do
          let delayMs = calculateBackoff policy attempt
          Flow.Class.Schedule.delay delayMs
          retryLoop handler policy inner a (attempt + 1)

calculateBackoff :: Flow.Types.RetryPolicy -> Int -> Flow.Types.Milliseconds
calculateBackoff policy attempt =
  let
    (Flow.Types.RetryPolicy p) = policy
    Flow.Types.Milliseconds initialMs = p.initialDelay
    Flow.Types.Milliseconds maxMs = p.maxDelay
    exponent = attempt - 1
    multiplier = numberPow p.backoffMultiplier exponent
    delayMs = Data.Int.floor (Data.Int.toNumber initialMs * multiplier)
    clampedMs = min maxMs delayMs
  in
    Flow.Types.Milliseconds clampedMs

numberPow :: Number -> Int -> Number
numberPow base exp
  | exp <= 0 = 1.0
  | otherwise = base * numberPow base (exp - 1)
