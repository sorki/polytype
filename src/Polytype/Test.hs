{-|
Description : Test effect

Contrived, needs work.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Polytype.Test where

import Polysemy
import Polysemy.Error
import Data.Kind (Type)
import qualified System.Exit

data Test (error :: Type) (m :: Type -> Type) (a :: Type) where
  TestPass :: z -> Test error m z
  TestFail :: error -> Test error m ()

makeSem_ ''Test

testPass :: forall z error r .  Member (Test error) r
         => z -> Sem r z

testFail :: forall error r .  Member (Test error) r
         => error
         -> Sem r ()

-- | Interpret `Test` failure as fatal error resulting
-- in `System.Exit.die`.
runTestIOFatal :: forall error r a .  Member (Embed IO) r
          => (error -> String)
          -> Sem (Test error ': r) a
          -> Sem r a
runTestIOFatal fmtError = interpret $ \case
  TestFail e -> embed $ System.Exit.die $ fmtError e
  TestPass ret -> return ret

-- | Reinterpret `Test` in terms of `Error` effect
runTestAsError :: forall e r a .  Member (Error e) r
          => Sem (Test e ': r) a
          -> Sem (Error e ': r) a
runTestAsError = reinterpret $ \case
  TestFail e -> throw e
  TestPass ret -> pure ret

{--
 - hard
runErrorAsTest :: forall e r a .  Member (Test e) r
          => Sem (Error e ': r) a
          -> Sem (Test e ': r) a
runErrorAsTest = reinterpretH $ \case
  Throw e -> testFail e >>= pureT
  -- Catch f -> pure ret

runTestPure ::  forall error r a . Show error
          => Sem (Test error ': r) a
          -> Sem r (Either error a)
runTestPure (Sem sem) = Sem $ \k -> sem $ \u ->
  case decomp u of
    _ -> undefined
--reinterpret $ \case
--  TestFail e -> pure $ Left e
--}


