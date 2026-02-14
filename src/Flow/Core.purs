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
  , leaf
  , mapArray
  , timeout
  , retry
  , defaultRetryPolicy
  ) where

import Prelude hiding ((>>>))

import Data.Either as Data.Either
import Data.Tuple as Data.Tuple
import Data.Variant as Data.Variant
import Flow.Types as Flow.Types

step :: forall i o a b. String -> Flow.Types.Workflow i o a b -> Flow.Types.Workflow i o a b
step name w = Flow.Types.Step name w

pure' :: forall i o a b. (a -> b) -> Flow.Types.Workflow i o a b
pure' = Flow.Types.Pure

infixr 1 compose as >>>

compose :: forall i o a b c. Flow.Types.Workflow i o a b -> Flow.Types.Workflow i o b c -> Flow.Types.Workflow i o a c
compose = Flow.Types.mkSeq

infixr 3 parallel as ***

parallel
  :: forall i o a1 a2 b1 b2
   . Flow.Types.Workflow i o a1 b1
  -> Flow.Types.Workflow i o a2 b2
  -> Flow.Types.Workflow i o (Data.Tuple.Tuple a1 a2) (Data.Tuple.Tuple b1 b2)
parallel = Flow.Types.mkPar

infixr 2 fanin as |||

fanin
  :: forall i o a b c
   . Flow.Types.Workflow i o a c
  -> Flow.Types.Workflow i o b c
  -> Flow.Types.Workflow i o (Data.Either.Either a b) c
fanin left right = Flow.Types.mkChoice identity left right

leaf
  :: forall i o a b s
   . String
  -> (a -> Flow.Types.LeafStep o s b)
  -> (s -> Data.Variant.Variant i -> Flow.Types.LeafStep o s b)
  -> Flow.Types.Workflow i o a b
leaf = Flow.Types.mkLeaf

mapArray
  :: forall i o x y
   . Flow.Types.Workflow i o x y
  -> Flow.Types.Workflow i o (Array x) (Array y)
mapArray = Flow.Types.mkMapArray

timeout
  :: forall i o a b
   . Flow.Types.Milliseconds
  -> Flow.Types.Workflow i o a b
  -> Flow.Types.Workflow i o a (Data.Either.Either Flow.Types.TimeoutError b)
timeout = Flow.Types.mkTimeout

retry
  :: forall i o a e b
   . Flow.Types.RetryPolicy
  -> Flow.Types.Workflow i o a (Data.Either.Either e b)
  -> Flow.Types.Workflow i o a (Flow.Types.RetryResult e b)
retry = Flow.Types.mkRetry

defaultRetryPolicy :: Flow.Types.RetryPolicy
defaultRetryPolicy = Flow.Types.RetryPolicy
  { maxAttempts: 3
  , initialDelay: Flow.Types.Milliseconds 1000
  , backoffMultiplier: 2.0
  , maxDelay: Flow.Types.Milliseconds 30000
  }
