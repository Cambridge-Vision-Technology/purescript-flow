module Flow.Class.Schedule
  ( class MonadSchedule
  , race
  , delay
  , parallel
  ) where

import Prelude

import Data.Either as Data.Either
import Data.Identity as Data.Identity
import Data.Tuple as Data.Tuple
import Flow.Types as Flow.Types

class Monad m <= MonadSchedule m where
  race :: forall a b. m a -> m b -> m (Data.Either.Either a b)
  delay :: Flow.Types.Milliseconds -> m Unit
  parallel :: forall a b. m a -> m b -> m (Data.Tuple.Tuple a b)

-- NOTE: This instance is suitable for unit testing pure workflow logic.
-- Real applications should use platform-specific instances (e.g., Aff).
instance MonadSchedule Data.Identity.Identity where
  race (Data.Identity.Identity a) _ = Data.Identity.Identity (Data.Either.Left a)
  delay _ = Data.Identity.Identity unit
  parallel (Data.Identity.Identity a) (Data.Identity.Identity b) =
    Data.Identity.Identity (Data.Tuple.Tuple a b)
