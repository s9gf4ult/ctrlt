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
import           Test.ShouldNotTypecheck
import           Test.Tasty
import           Test.Tasty.HUnit

test_CallCC_Escape_Not_Allowed :: TestTree
test_CallCC_Escape_Not_Allowed = testGroup "CallCC escape not allowed"
  [ testCase "CtrlT" (noEscapeCC evalCtrlT)
  , testCase "EscT" (noEscapeCC $ fmap (either id id) . evalEscT)
  , testCase "SpaceT" (noEscapeCC $ evalCtrlT . evalSpaceT ())
  ]

test_No_Coerce :: TestTree
test_No_Coerce = testGroup "CallCC escape not coercible"
  [ testCase "CtrlT" (noCoerceCC evalCtrlT)
  , testCase "EscT" (noCoerceCC $ fmap (either id id) . evalEscT)
  , testCase "SpaceT" (noCoerceCC $ evalCtrlT . evalSpaceT ())
  ]

noEscapeCC
  :: (Phoenix c IO, MonadContC c IO, MonadThrowC c IO)
  => Eval c IO Int
  -> Assertion
noEscapeCC evalC = do
  res <- try $ evalC $ do
    callCC $ \esc -> do
      indexedCatchAll (esc 1 *> return 2) throwM
                 -- Rethrowing type error ^^^
  shouldNotTypecheck $ case res of
    Left (e :: SomeException) -> throw e
    Right a                   -> a

noCoerceCC
  :: (Phoenix c IO, MonadContC c IO, MonadThrowC c IO)
  => Eval c IO Int
  -> Assertion
noCoerceCC evalC = do
  res <- try $ evalC $ do
    callCC $ \esc -> do
      indexedCatchAll (coerce (esc 1) *> return 2) throwM
                 -- Rethrowing type error ^^^
  shouldNotTypecheck $ case res of
    Left (e :: SomeException) -> throw e
    Right a                   -> a
