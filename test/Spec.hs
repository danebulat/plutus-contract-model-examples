{-# LANGUAGE ImportQualifiedPost #-}

module Main where 

import Spec.GuessGameV1 qualified as GuessGameV1
import Spec.GuessGameV2 qualified as GuessGameV2
import Test.Tasty
import System.Environment

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS"   "100"
  setEnv "TASTY_QUICKCHECK_VERBOSE" "true"

  defaultMain tests

  unsetEnv "TASTY_QUICKCHECK_TESTS"
  unsetEnv "TASTY_QUICKCHECK_VERBOSE"

tests :: TestTree 
tests = testGroup "guess game" 
  [ GuessGameV1.tests 
  , GuessGameV2.tests
  ]
