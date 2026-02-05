-- | Composition primitives for Workflow.
-- |
-- | This module provides convenient re-exports of the standard composition
-- | typeclasses and their operators. Workflow instances are defined in
-- | Flow.Types to avoid orphan instances.
-- |
-- | Workflow implements:
-- | - Semigroupoid/Category for sequential composition (>>>)
-- | - Profunctor for mapping over input/output (dimap, lcmap, rmap)
-- | - Strong for parallel composition (first, second)
-- | - Choice for branching (left, right)
-- |
-- | Usage:
-- | ```purescript
-- | import Flow (Workflow)
-- | import Flow.Compose
-- |
-- | myWorkflow :: Workflow () () Int String
-- | myWorkflow = doublePure >>> showPure
-- | ```
module Flow.Compose
  ( module Control.Category
  , module Control.Semigroupoid
  , module Data.Profunctor
  , module Data.Profunctor.Choice
  , module Data.Profunctor.Strong
  ) where

import Control.Category (class Category, identity, (<<<), (>>>)) as Control.Category
import Control.Semigroupoid (class Semigroupoid, compose) as Control.Semigroupoid
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap) as Data.Profunctor
import Data.Profunctor.Choice (class Choice, left, right, (+++)) as Data.Profunctor.Choice
import Data.Profunctor.Strong (class Strong, first, second) as Data.Profunctor.Strong
