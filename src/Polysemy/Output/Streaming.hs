-- bit contrived
--
module Polysemy.Output.Streaming
  ( -- * Underlying Effect
    module Polysemy.Output

  , runOutputStream
  , runOutputStreamStdout
  ) where

import           Polysemy
import           Polysemy.Output

import           Streaming.Prelude (Stream, Of)
import qualified Streaming.Prelude

runOutputStream :: forall stringy m r a . Monad m
                => (Stream (Of stringy) m () -> Sem r ())
                -> Sem (Output stringy ': r) a
                -> Sem r a
runOutputStream f = runOutputSem $ f . Streaming.Prelude.yield

runOutputStreamStdout :: Member (Embed IO) r => InterpreterFor (Output String) r
runOutputStreamStdout = runOutputSem (\s ->
  Streaming.Prelude.stdoutLn (Streaming.Prelude.yield s))
