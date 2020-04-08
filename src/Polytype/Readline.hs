{-|
Description : Readline effect

Like `Control.Effect.Readline` from fused-effects-readline
.. but wannabe polymorphic
* XXX: haskeline-polysemy /o\ \o/ /o\ \o/
-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polytype.Readline where

--import Data.Text.Prettyprint.Doc (Doc)
--import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Prelude hiding (print)

import Polysemy
import Data.Kind (Type)

import System.Console.Haskeline

data Readline (stringy :: Type) (m :: Type -> Type) (a :: Type) where
  Prompt :: stringy -> Readline stringy m (Maybe stringy)
  Print :: stringy -> Readline stringy m ()
  --Print :: (Doc AnsiStyle) -> Readline stringy m (Doc AnsiStyle)

makeSem_ ''Readline

prompt :: forall stringy r .  Member (Readline stringy) r
       => stringy                -- ^ Prompt string
       -> Sem r (Maybe stringy)  -- ^ User input

print  :: forall stringy r .  Member (Readline stringy) r
       => stringy                -- ^ Ouput message
       -> Sem r ()

runReadlineHaskeline :: Member (Embed IO) r
                     => Sem (Readline String ': r) a
                     -> Sem r a
runReadlineHaskeline = interpret $ \case
  Prompt p -> embed $ runInputT defaultSettings
    $ getInputLine p
  Print p -> embed $ runInputT defaultSettings $ outputStrLn p


test_repl :: IO ()
test_repl = runM . runReadlineHaskeline $ do
  mu <- prompt "Username: "
  case mu of
    Nothing -> print "No input :("
    Just u -> do
      print $ "o/ " ++ u
      _ <- prompt ("[" ++ u ++ "]> ")
      return ()
