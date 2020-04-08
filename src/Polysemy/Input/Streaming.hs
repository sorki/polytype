-- borrowed from polysemy-zoo
--
module Polysemy.Input.Streaming
  ( -- * Underlying Effect
    module Polysemy.Input

    -- * Actions
  , yieldInput
  , yieldRace
  , exhaust

    -- * Intepretations
  , runInputViaStream
  , runInputViaInfiniteStream

  , runOutputStream
  , runOutputStreamStdout
  ) where

import qualified Control.Concurrent.Async as A
import           Data.Functor.Of
import           Data.Void
import           Polysemy
import           Polysemy.Final
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.State
import           Streaming (Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Streaming.Prelude


-- runM . runOutputSem (\s -> Streaming.Prelude.stdoutLn (Streaming.Prelude.yield s))  . runInputViaStream Streaming.Prelude.stdinLn  . runTeletypeInputOutput @String $ readTTY @String
--
--runOutputStream :: Member (Embed IO) r => InterpreterFor (Output String) r
runOutputStream :: forall stringy m r a . Monad m
                => (Stream (Of stringy) m () -> Sem r ())
                -> Sem (Output stringy ': r) a
                -> Sem r a
runOutputStream f = runOutputSem $ f . Streaming.Prelude.yield

runOutputStreamStdout :: Member (Embed IO) r => InterpreterFor (Output String) r
runOutputStreamStdout = runOutputSem (\s ->
  Streaming.Prelude.stdoutLn (Streaming.Prelude.yield s))
{--
runOutputViaStream 
    :: forall r i . (i -> Sem r ()) -- (S.Stream (Of i) (Sem r) () -> Sem r ())
    -> InterpreterFor (Output i) r
--}

runInputViaStream
    :: S.Stream (Of i) (Sem r) ()
    -> InterpreterFor (Input (Maybe i)) r
runInputViaStream stream
  = evalState (Just stream)
  . reinterpret ( \Input ->
      get >>= \case
        Nothing -> pure Nothing
        Just s ->
          raise (S.inspect s) >>= \case
            Left () -> pure Nothing
            Right (i :> s') -> do
              put $ Just s'
              pure $ Just i
  )


runInputViaInfiniteStream
    :: forall i r
     . S.Stream (Of i) (Sem r) Void
    -> InterpreterFor (Input i) r
runInputViaInfiniteStream stream
  = evalState stream
  . reinterpret ( \Input -> do
      s <- get
      raise (S.inspect s) >>= \case
        Left g -> absurd g
        Right (i :> s') -> do
          put s'
          pure i
  )


yieldRace
    :: Members
        '[ Final IO
         , Input i1
         , Input i2
         ] r
    => S.Stream (S.Of (Either i1 i2)) (Sem r) ()
yieldRace = do
  z <- S.lift $ withStrategicToFinal $ do
         input1 <- runS input
         input2 <- runS input
         pure $ fmap sequenceEither $ A.race input1 input2
  S.yield z


sequenceEither :: Functor f => Either (f a) (f b) -> f (Either a b)
sequenceEither (Left fa) = Left <$> fa
sequenceEither (Right fb) = Right <$> fb


yieldInput :: Member (Input i) r => S.Stream (Of i) (Sem r) ()
yieldInput = S.lift input >>= S.yield


exhaust :: Member (Input i) r => S.Stream (Of i) (Sem r) a
exhaust = S.repeatM input
