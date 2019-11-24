{-# OPTIONS_GHC -fdefer-type-errors #-}

module TypeTest where

import           Control.Exception
import           Control.Monad.Catch       (throwM)
import           Control.Monad.Cont
import           Control.Monad.CtrlT
import           Control.Monad.CtrlT.Class
import           Control.Monad.EscT
import           Control.Monad.SpaceT
import           Data.Coerce
import           GHC.Stack
import           Test.ShouldNotTypecheck
import           Test.Tasty
import           Test.Tasty.HUnit

test_No_CC :: HasCallStack => TestTree
test_No_CC = testGroup "CallCC escape not allowed"
  [ testCase "CtrlT" $ do
      res <- try $ evalCtrlT $ do
        callCC $ \esc -> do
          indexedCatchAll (esc 1 *> return 2) throwM
                     -- Rethrowing type error ^^^
      shouldNotTypecheck $ case res of
        Left (e :: SomeException) -> throw e
        Right (a :: Int)          -> a
  , testCase "EscT" $ do
      res <- try $ evalCtrlT $ evalEscT $ do
        callCC $ \esc -> do
          indexedCatchAll (esc 1 *> return 2) throwM
                     -- Rethrowing type error ^^^
      shouldNotTypecheck $ case res of
        Left (e :: SomeException)   -> throw e
        Right (a :: Either Int Int) -> a
  , testCase "SpaceT" $ do
      res <- try $ evalCtrlT $ evalSpaceT () $ do
        callCC $ \esc -> do
          indexedCatchAll (esc 1 *> return 2) throwM
                     -- Rethrowing type error ^^^
      shouldNotTypecheck $ case res of
        Left (e :: SomeException) -> throw e
        Right (a :: Int)          -> a
  ]


test_No_Coerce :: HasCallStack => TestTree
test_No_Coerce = testGroup "CallCC escape not coercible"
  [ testCase "CtrlT" $ do
      res <- try $ evalCtrlT $ do
        callCC $ \esc -> do
          indexedCatchAll (coerce (esc 1) *> return 2) throwM
                              -- Rethrowing type error ^^^
      shouldNotTypecheck $ case res of
        Left (e :: SomeException) -> throw e
        Right (a :: Int)          -> a
  , testCase "EscT" $ do
      res <- try $ evalCtrlT $ evalEscT $ do
        callCC $ \esc -> do
          indexedCatchAll (coerce (esc 1) *> return 2) throwM
                              -- Rethrowing type error ^^^
      shouldNotTypecheck $ case res of
        Left (e :: SomeException)   -> throw e
        Right (a :: Either Int Int) -> a
  , testCase "SpaceT" $ do
      res <- try $ evalCtrlT $ evalSpaceT () $ do
        callCC $ \esc -> do
          indexedCatchAll (coerce (esc 1) *> return 2) throwM
                              -- Rethrowing type error ^^^
      shouldNotTypecheck $ case res of
        Left (e :: SomeException) -> throw e
        Right (a :: Int)          -> a
  ]
