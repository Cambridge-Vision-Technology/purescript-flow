module Flow.Types
  ( Workflow(..)
  , SeqF(..)
  , ParCPS(..)
  , ChoiceCPS(..)
  , RequestCPS(..)
  , MapArrayCPS(..)
  , TimeoutCPS(..)
  , RetryCPS(..)
  , mkSeq
  , mkPar
  , mkChoice
  , mkRequest
  , mkMapArray
  , mkTimeout
  , mkRetry
  , Milliseconds(..)
  , TimeoutError(..)
  , RetryPolicy(..)
  , RetryResult(..)
  ) where

import Prelude

import Control.Category as Control.Category
import Control.Semigroupoid as Control.Semigroupoid
import Data.Either as Data.Either
import Data.Exists as Data.Exists
import Data.Functor.Variant as Data.Functor.Variant
import Data.Newtype as Data.Newtype
import Data.Profunctor as Data.Profunctor
import Data.Profunctor.Choice as Data.Profunctor.Choice
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.Tuple as Data.Tuple

newtype Milliseconds = Milliseconds Int

derive instance Data.Newtype.Newtype Milliseconds _
derive instance Eq Milliseconds
derive instance Ord Milliseconds

instance Show Milliseconds where
  show (Milliseconds ms) = show ms <> "ms"

data TimeoutError = TimedOut Milliseconds

derive instance Eq TimeoutError

instance Show TimeoutError where
  show (TimedOut duration) = "TimedOut " <> show duration

newtype RetryPolicy = RetryPolicy
  { maxAttempts :: Int
  , initialDelay :: Milliseconds
  , backoffMultiplier :: Number
  , maxDelay :: Milliseconds
  }

derive instance Data.Newtype.Newtype RetryPolicy _
derive instance Eq RetryPolicy

instance Show RetryPolicy where
  show (RetryPolicy p) =
    "RetryPolicy { maxAttempts: " <> show p.maxAttempts
      <> ", initialDelay: "
      <> show p.initialDelay
      <> ", backoffMultiplier: "
      <> show p.backoffMultiplier
      <> ", maxDelay: "
      <> show p.maxDelay
      <> " }"

newtype RetryResult e a = RetryResult { attempts :: Int, result :: Data.Either.Either e a }

derive instance Data.Newtype.Newtype (RetryResult e a) _
derive instance (Eq e, Eq a) => Eq (RetryResult e a)

instance (Show e, Show a) => Show (RetryResult e a) where
  show (RetryResult r) =
    "RetryResult { attempts: " <> show r.attempts
      <> ", result: "
      <> show r.result
      <> " }"

-- NOTE: Seq uses Data.Exists for a single-variable existential (the intermediate type x),
-- which is the idiomatic PureScript approach. Par, Choice, Request, Timeout, and Retry use
-- CPS encoding instead because they hide multiple existential variables along with accessor
-- functions — CPS is more natural when the existential payload includes both types and
-- functions that operate on those types.
data Workflow (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b
  = Pure (a -> b)
  | Step String (Workflow i o a b)
  | Seq (Data.Exists.Exists (SeqF i o a b))
  | Par (ParCPS i o a b)
  | Choice (ChoiceCPS i o a b)
  | Request String (RequestCPS i o a b)
  | MapArray (MapArrayCPS i o a b)
  | Timeout (TimeoutCPS i o a b)
  | Retry (RetryCPS i o a b)

data SeqF (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b x = SeqF (Workflow i o a x) (Workflow i o x b)

mkSeq :: forall i o a b x. Workflow i o a x -> Workflow i o x b -> Workflow i o a b
mkSeq w1 w2 = Seq (Data.Exists.mkExists (SeqF w1 w2))

newtype ParCPS (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b = ParCPS
  ( forall r
     . ( forall a1 b1 a2 b2
          . (a -> a1)
         -> (a -> a2)
         -> (b1 -> b2 -> b)
         -> Workflow i o a1 b1
         -> Workflow i o a2 b2
         -> r
       )
    -> r
  )

mkPar
  :: forall i o a1 a2 b1 b2
   . Workflow i o a1 b1
  -> Workflow i o a2 b2
  -> Workflow i o (Data.Tuple.Tuple a1 a2) (Data.Tuple.Tuple b1 b2)
mkPar w1 w2 = Par (ParCPS (\k -> k fst' snd' Data.Tuple.Tuple w1 w2))
  where
  fst' :: Data.Tuple.Tuple a1 a2 -> a1
  fst' (Data.Tuple.Tuple a _) = a

  snd' :: Data.Tuple.Tuple a1 a2 -> a2
  snd' (Data.Tuple.Tuple _ a) = a

newtype ChoiceCPS (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b = ChoiceCPS
  ( forall r
     . ( forall x y
          . (a -> Data.Either.Either x y)
         -> Workflow i o x b
         -> Workflow i o y b
         -> r
       )
    -> r
  )

mkChoice
  :: forall i o a b x y
   . (a -> Data.Either.Either x y)
  -> Workflow i o x b
  -> Workflow i o y b
  -> Workflow i o a b
mkChoice splitFn leftW rightW = Choice (ChoiceCPS (\k -> k splitFn leftW rightW))

newtype RequestCPS (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b = RequestCPS
  ( forall r
     . ( forall x
          . (a -> Data.Functor.Variant.VariantF o x)
         -> (x -> Workflow i o x b)
         -> r
       )
    -> r
  )

mkRequest
  :: forall i o a b x
   . String
  -> (a -> Data.Functor.Variant.VariantF o x)
  -> (x -> Workflow i o x b)
  -> Workflow i o a b
mkRequest label toRequest fromResponse = Request label (RequestCPS (\k -> k toRequest fromResponse))

newtype MapArrayCPS (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b = MapArrayCPS
  ( forall r
     . ( forall x y
          . Workflow i o x y
         -> (a -> Array x)
         -> (Array y -> b)
         -> r
       )
    -> r
  )

mkMapArray
  :: forall i o x y
   . Workflow i o x y
  -> Workflow i o (Array x) (Array y)
mkMapArray inner = MapArray (MapArrayCPS (\k -> k inner identity identity))

newtype TimeoutCPS (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b = TimeoutCPS
  ( forall r
     . ( forall innerB
          . Milliseconds
         -> Workflow i o a innerB
         -> (Data.Either.Either TimeoutError innerB -> b)
         -> r
       )
    -> r
  )

mkTimeout
  :: forall i o a b
   . Milliseconds
  -> Workflow i o a b
  -> Workflow i o a (Data.Either.Either TimeoutError b)
mkTimeout duration inner = Timeout (TimeoutCPS (\k -> k duration inner identity))

newtype RetryCPS (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b = RetryCPS
  ( forall r
     . ( forall e innerB
          . RetryPolicy
         -> Workflow i o a (Data.Either.Either e innerB)
         -> (RetryResult e innerB -> b)
         -> r
       )
    -> r
  )

mkRetry
  :: forall i o a e b
   . RetryPolicy
  -> Workflow i o a (Data.Either.Either e b)
  -> Workflow i o a (RetryResult e b)
mkRetry policy inner = Retry (RetryCPS (\k -> k policy inner identity))

instance Control.Semigroupoid.Semigroupoid (Workflow i o) where
  compose w2 w1 = mkSeq w1 w2

instance Control.Category.Category (Workflow i o) where
  identity = Pure identity

instance Data.Profunctor.Profunctor (Workflow i o) where
  dimap f g w = mkSeq (Pure f) (mkSeq w (Pure g))

instance Data.Profunctor.Strong.Strong (Workflow i o) where
  first w = mkPar w (Pure identity)
  second w = mkPar (Pure identity) w

instance Data.Profunctor.Choice.Choice (Workflow i o) where
  left w = mkChoice identity (mkSeq w (Pure Data.Either.Left)) (Pure Data.Either.Right)
  right w = mkChoice identity (Pure Data.Either.Left) (mkSeq w (Pure Data.Either.Right))
