module Test.BDD where

import Prelude

import Test.Spec as Test.Spec

feature :: String -> Test.Spec.Spec Unit -> Test.Spec.Spec Unit
feature = Test.Spec.describe

scenario :: String -> Test.Spec.Spec Unit -> Test.Spec.Spec Unit
scenario name = Test.Spec.describe ("Scenario: " <> name)
