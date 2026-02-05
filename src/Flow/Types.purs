module Flow.Types
  ( Workflow(..)
  , SeqF(..)
  , ParCPS(..)
  , ChoiceCPS(..)
  , RequestCPS(..)
  , mkSeq
  , mkPar
  , mkChoice
  , mkRequest
  ) where

import Prelude

import Control.Category as Control.Category
import Control.Semigroupoid as Control.Semigroupoid
import Data.Either as Data.Either
import Data.Exists as Data.Exists
import Data.Functor.Variant as Data.Functor.Variant
import Data.Profunctor as Data.Profunctor
import Data.Profunctor.Choice as Data.Profunctor.Choice
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.Tuple as Data.Tuple

-- | The core workflow type representing an inspectable, composable computation.
-- |
-- | Type parameters:
-- | - `i`: input messages (responses workflow RECEIVES) - row of functors
-- | - `o`: output messages (requests workflow SENDS) - row of functors
-- | - `a`: input value (workflow starts with)
-- | - `b`: output value (workflow ends with)
-- |
-- | The row parameters `i` and `o` are rows of functors (Type -> Type) suitable
-- | for use with VariantF. Each label maps to a functor that represents an effect.
-- |
-- | This is a free-ish structure that can be interpreted in multiple ways:
-- | - Execute with handlers (runtime)
-- | - Generate diagrams (static analysis)
-- | - Optimize/transform (compiler passes)
-- |
-- | NOTE: Par and Choice constructors use CPS-style encoding to hide existential types.
-- | The interpreter extracts the inner workflows via the provided continuations.
data Workflow (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b
  = Pure (a -> b)
  | Step String (Workflow i o a b)
  | Seq (Data.Exists.Exists (SeqF i o a b))
  | Par (ParCPS i o a b)
  | Choice (ChoiceCPS i o a b)
  | Request (RequestCPS i o a b)

-- | Sequential composition: a -> x -> b
-- | The type parameter `x` is the intermediate type being hidden by Exists.
data SeqF (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b x = SeqF (Workflow i o a x) (Workflow i o x b)

-- | Smart constructor for sequential composition.
mkSeq :: forall i o a b x. Workflow i o a x -> Workflow i o x b -> Workflow i o a b
mkSeq w1 w2 = Seq (Data.Exists.mkExists (SeqF w1 w2))

-- | CPS-encoded parallel composition.
-- | Hides the intermediate types a1, b1, a2, b2 using continuation-passing style.
-- | The continuation receives both workflows and type-safe accessor functions.
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

-- | Smart constructor for parallel composition on tuples.
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

-- | CPS-encoded choice composition.
-- | Hides the branch types x and y using continuation-passing style.
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

-- | Smart constructor for choice/branching.
mkChoice
  :: forall i o a b x y
   . (a -> Data.Either.Either x y)
  -> Workflow i o x b
  -> Workflow i o y b
  -> Workflow i o a b
mkChoice splitFn leftW rightW = Choice (ChoiceCPS (\k -> k splitFn leftW rightW))

-- | CPS-encoded effectful request.
-- | Hides the intermediate request/response type using continuation-passing style.
-- |
-- | The continuation receives:
-- | - A function to build the request VariantF from input
-- | - A function to continue the workflow with the response value
-- |
-- | Type parameters:
-- | - `i`: row of functors for incoming messages (unused in current design, for future expansion)
-- | - `o`: row of functors for outgoing messages (requests)
-- | - `a`: workflow input type
-- | - `b`: workflow output type
-- |
-- | The hidden type `x` represents the expected response type from the effect.
-- | The handler interprets `VariantF o x` and produces an `x` value.
newtype RequestCPS (i :: Row (Type -> Type)) (o :: Row (Type -> Type)) a b = RequestCPS
  ( forall r
     . ( forall x
          . (a -> Data.Functor.Variant.VariantF o x)
         -> (x -> Workflow i o x b)
         -> r
       )
    -> r
  )

-- | Smart constructor for effectful requests.
-- |
-- | Creates a workflow that:
-- | 1. Takes input `a`
-- | 2. Builds a request using `toRequest` to produce a `VariantF o x`
-- | 3. Handler performs effect and returns response of type `x`
-- | 4. Continues with the workflow produced by `fromResponse x`
-- |
-- | Example:
-- | ```purescript
-- | -- Define an effect type
-- | data HttpF a = Get String (String -> a)
-- | type HTTP r = (http :: HttpF | r)
-- |
-- | -- Create a workflow that fetches a URL
-- | fetchUrl :: forall i. Workflow i (HTTP ()) String String
-- | fetchUrl = mkRequest
-- |   (\url -> VariantF.inj (Proxy :: _ "http") (Get url identity))
-- |   (\body -> Pure identity)  -- body :: String, return it unchanged
-- | ```
-- |
-- | The handler must interpret the VariantF and produce the expected response:
-- | ```purescript
-- | handleHttp :: VariantF (HTTP ()) x -> m x
-- | handleHttp = VariantF.on (Proxy :: _ "http")
-- |   (\(Get url k) -> do
-- |     body <- httpGet url
-- |     pure (k body))  -- k transforms response to x
-- |   VariantF.case_
-- | ```
mkRequest
  :: forall i o a b x
   . (a -> Data.Functor.Variant.VariantF o x)
  -> (x -> Workflow i o x b)
  -> Workflow i o a b
mkRequest toRequest fromResponse = Request (RequestCPS (\k -> k toRequest fromResponse))

-- | Semigroupoid instance for sequential composition.
-- | compose w2 w1 runs w1 first, then w2 (i.e., w1 >>> w2).
instance Control.Semigroupoid.Semigroupoid (Workflow i o) where
  compose w2 w1 = mkSeq w1 w2

-- | Category instance provides the identity workflow.
-- | identity passes input through unchanged.
instance Control.Category.Category (Workflow i o) where
  identity = Pure identity

-- | Profunctor instance allows mapping over input and output types.
-- | dimap f g w applies f to input before w, and g to output after w.
instance Data.Profunctor.Profunctor (Workflow i o) where
  dimap f g w = mkSeq (Pure f) (mkSeq w (Pure g))

-- | Strong instance enables parallel composition with tuple passing.
-- | first w runs w on the first element, passes second element through.
-- | second w runs w on the second element, passes first element through.
instance Data.Profunctor.Strong.Strong (Workflow i o) where
  first w = mkPar w (Pure identity)
  second w = mkPar (Pure identity) w

-- | Choice instance enables branching based on Either values.
-- | left w: given Either a c, run w on Left a to get b, wrap as Either b c
-- | right w: given Either a b, run w on Right b to get c, wrap as Either a c
instance Data.Profunctor.Choice.Choice (Workflow i o) where
  left w = mkChoice identity (mkSeq w (Pure Data.Either.Left)) (Pure Data.Either.Right)
  right w = mkChoice identity (Pure Data.Either.Left) (mkSeq w (Pure Data.Either.Right))
