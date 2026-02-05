module Test.Compose where

import Prelude

import Control.Category as Control.Category
import Data.Either as Data.Either
import Data.Profunctor as Data.Profunctor
import Data.Profunctor.Choice as Data.Profunctor.Choice
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.Tuple as Data.Tuple
import Flow.Types as Flow.Types

-- | Test workflows for verification
-- | These tests verify that composition primitives compile with correct types
-- | and produce the expected Workflow structure.

-- | A simple workflow that doubles an integer
doubleW :: forall i o. Flow.Types.Workflow i o Int Int
doubleW = Flow.Types.Pure (_ * 2)

-- | A simple workflow that adds 10
addTenW :: forall i o. Flow.Types.Workflow i o Int Int
addTenW = Flow.Types.Pure (_ + 10)

-- | A workflow that converts Int to String
showW :: forall i o. Flow.Types.Workflow i o Int String
showW = Flow.Types.Pure show

-- | Test: identity >>> w == w (structurally produces Seq)
-- | Since Workflow is data, we verify the types match and composition works.
testLeftIdentity :: forall i o. Flow.Types.Workflow i o Int Int
testLeftIdentity = Control.Category.identity Control.Category.>>> doubleW

-- | Test: w >>> identity == w (structurally produces Seq)
testRightIdentity :: forall i o. Flow.Types.Workflow i o Int Int
testRightIdentity = doubleW Control.Category.>>> Control.Category.identity

-- | Test: associativity - both produce valid workflows with same type
testAssociativityLeft :: forall i o. Flow.Types.Workflow i o Int String
testAssociativityLeft = (doubleW Control.Category.>>> addTenW) Control.Category.>>> showW

testAssociativityRight :: forall i o. Flow.Types.Workflow i o Int String
testAssociativityRight = doubleW Control.Category.>>> (addTenW Control.Category.>>> showW)

-- | Test: Profunctor dimap
testDimap :: forall i o. Flow.Types.Workflow i o String String
testDimap = Data.Profunctor.dimap stringToInt intToString doubleW
  where
  stringToInt :: String -> Int
  stringToInt _ = 0

  intToString :: Int -> String
  intToString = show

-- | Test: Strong first - runs workflow on first element of tuple
testFirst :: forall i o. Flow.Types.Workflow i o (Data.Tuple.Tuple Int String) (Data.Tuple.Tuple Int String)
testFirst = Data.Profunctor.Strong.first doubleW

-- | Test: Strong second - runs workflow on second element of tuple
testSecond :: forall i o. Flow.Types.Workflow i o (Data.Tuple.Tuple String Int) (Data.Tuple.Tuple String Int)
testSecond = Data.Profunctor.Strong.second doubleW

-- | Test: Choice left - runs workflow on Left values
testLeft :: forall i o. Flow.Types.Workflow i o (Data.Either.Either Int String) (Data.Either.Either Int String)
testLeft = Data.Profunctor.Choice.left doubleW

-- | Test: Choice right - runs workflow on Right values
testRight :: forall i o. Flow.Types.Workflow i o (Data.Either.Either String Int) (Data.Either.Either String Int)
testRight = Data.Profunctor.Choice.right doubleW

-- | Test: Composition of Strong workflows (parallel composition via ***)
testParallelComposition
  :: forall i o
   . Flow.Types.Workflow i o (Data.Tuple.Tuple Int Int) (Data.Tuple.Tuple Int Int)
testParallelComposition =
  Data.Profunctor.Strong.first doubleW
    Control.Category.>>> Data.Profunctor.Strong.second addTenW

-- | Test: fanout (&&&) pattern - same input to two workflows, combine results
testFanout :: forall i o. Flow.Types.Workflow i o Int (Data.Tuple.Tuple Int Int)
testFanout =
  Data.Profunctor.dimap (\x -> Data.Tuple.Tuple x x) identity
    (Data.Profunctor.Strong.first doubleW Control.Category.>>> Data.Profunctor.Strong.second addTenW)

-- | Test: fanin (|||) pattern using Choice
testFanin
  :: forall i o
   . Flow.Types.Workflow i o (Data.Either.Either Int Int) Int
testFanin =
  Data.Profunctor.Choice.left doubleW
    Control.Category.>>> Data.Profunctor.Choice.right addTenW
    Control.Category.>>> Flow.Types.Pure mergeEither
  where
  mergeEither :: Data.Either.Either Int Int -> Int
  mergeEither (Data.Either.Left n) = n
  mergeEither (Data.Either.Right n) = n

-- | Test: Named steps for diagram generation
testNamedSteps :: forall i o. Flow.Types.Workflow i o Int Int
testNamedSteps =
  Flow.Types.Step "Double" doubleW
    Control.Category.>>> Flow.Types.Step "Add Ten" addTenW
