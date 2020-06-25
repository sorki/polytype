{-|
Description :  Polymorphic Teletype effect

Polymorphic Teletype so we can use different kinds of terminals
  * line based
  * character based
  * ByteString/Text based
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polytype.Teletype (
    Teletype(..)
  , readTTY
  , writeTTY

  , echo

  , teletypeToIO
  , runTeletypePure
  , runTeletypeAsInputOutput
  , runTeletypeStreaming
  , runTeletypeStreaming'
  , runTeletypeStreamingStdio

  , convertTeletypeStrings
  , bufferBy
  , mapTeletype
  , mapMTeletype
  , teletypeReadOnly
  , teletypeParseWith

  , streamingInteract
  , streamingEcho
  ) where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State

import Polysemy.Input.Streaming

import Data.Attoparsec.Types (Parser, IResult(..))
import Data.Kind (Type)
import Data.String.Conversions (ConvertibleStrings)
import Streaming (Stream, Of)

import qualified Control.Monad
import qualified Data.String.Conversions
import qualified Streaming.Prelude

data Teletype (stringy :: Type) (m :: Type -> Type) (a :: Type) where
  ReadTTY  :: Teletype stringy m stringy
  WriteTTY :: stringy -> Teletype stringy m ()

makeSem_ ''Teletype

writeTTY :: forall stringy r
          . Member (Teletype stringy) r
         => stringy        -- ^ Message to write
         -> Sem r ()       -- ^ Effectful computation with no result

readTTY :: forall stringy r .  Member (Teletype stringy) r
        => Sem r stringy

-- | Run `Teletype` using line based IO.
teletypeToIO :: Member (Embed IO) r
             => Sem (Teletype String ': r) a
             -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

-- | Run `Teletype` purely.
--
-- >>> fst . run $ runTeletypePure ["ab", "cd"] $ echo @String
-- ["ab","cd"]
runTeletypePure :: forall stringy r a . Monoid stringy
                => [stringy]
                -> Sem (Teletype stringy ': r) a
                -> Sem r ([stringy], a)
runTeletypePure i
  = runOutputMonoid @stringy pure
  . runInputList i
  . runTeletypeAsInputOutput

-- | Run `Teletype` utilizing `Streaming`.
runTeletypeStreaming :: forall stringy r m a . (Monad m, Monoid stringy)
                     => (Stream (Of stringy) m () -> Sem r ())
                     -> Stream (Of stringy) (Sem (Output stringy ': r)) ()
                     -> Sem (Teletype stringy ': r) a
                     -> Sem r a
runTeletypeStreaming out instream =
   runOutputStream out
 . runInputViaStream @stringy instream
 . runTeletypeAsInputOutput @stringy

-- | Like `runTeletypeStreaming` but only with streaming input,
-- while output is collected via `runOutputList`.
runTeletypeStreaming' :: forall stringy r a . Monoid stringy
                      => Stream (Of stringy) (Sem (Output stringy ': r)) ()
                      -> Sem (Teletype stringy ': r) a
                      -> Sem r  ([stringy], a)
runTeletypeStreaming' s
  = runOutputList
  . runInputViaStream s
  . runTeletypeAsInputOutput

-- | Run `Teletype` using `Streaming.Prelude.stdoutLn`
-- and `Streaming.Prelude.stdinLn`.
runTeletypeStreamingStdio :: forall r a
                           . Member (Embed IO) r
                          => Sem (Teletype String ': r) a
                          -> Sem r a
runTeletypeStreamingStdio = runTeletypeStreaming
  Streaming.Prelude.stdoutLn
  Streaming.Prelude.stdinLn

-- Like `interact` but on top of teletype utilizing Streaming
streamingInteract :: forall a .
                     ( ConvertibleStrings String a
                     , ConvertibleStrings a String)
                  => (a -> a)
                  -> IO a
streamingInteract f =
   runM
 . runTeletypeStreaming Streaming.Prelude.stdoutLn Streaming.Prelude.stdinLn
 . convertTeletypeStrings @a @String
 $ Control.Monad.forever $ readTTY @a >>= writeTTY @a . f

-- | Like `echo` but implemented using `streamingInteract` without termination.
streamingEcho :: IO String
streamingEcho = streamingInteract @String id

-- | Echo `Teletype` reads as writes, terminate on empty read.
echo :: forall stringy r
      . (Eq stringy, Monoid stringy, Member (Teletype stringy) r)
     => Sem r ()
echo = do
  x <- readTTY @stringy
  case x of
    y | y == mempty -> pure ()
    _ -> do
      writeTTY x
      -- when we enable recursion we get
      -- ghc: panic! (the 'impossible' happened)
      --  (GHC version 8.10.1: mergeSATInfo
      --  Left:STSTSVSVSVSVSTSVSVSV, Right:STSTSVSVSVSCSTSVSVSV
      --
      echo @stringy

-- | Reinterpret `Teletype` in form of `Input` and `Output`
runTeletypeAsInputOutput :: forall stringy r a . Monoid stringy
                         => Sem (Teletype stringy ': r) a
                         -> Sem (Input (Maybe stringy) ': Output stringy ': r) a
runTeletypeAsInputOutput = reinterpret2 $ \case
      ReadTTY -> maybe mempty id <$> input -- @stringy
      WriteTTY msg -> output msg

-- | Convert from one string like `Teletype` to another utilizing
-- `Data.String.Conversions.convertString`.
convertTeletypeStrings :: forall stringy strlike r a .
                          ( ConvertibleStrings stringy strlike
                          , ConvertibleStrings strlike stringy)
                       => Sem (Teletype stringy ': r) a
                       -> Sem (Teletype strlike ': r) a
convertTeletypeStrings = reinterpret $ \case
  ReadTTY -> Data.String.Conversions.convertString <$> readTTY @strlike
  WriteTTY msg -> writeTTY @strlike $ Data.String.Conversions.convertString msg

-- | Provided a function like `lines` or `words` split the reads and writes
-- using state as a buffer internally.
bufferBy :: forall stringy r a .
              ( Eq stringy
              , Monoid stringy
              , Member (State [stringy]) r
              )
         => (stringy -> [stringy])
         -> Sem (Teletype stringy ': r) a
         -> Sem (Teletype stringy ': r) a
bufferBy splitter = reinterpret $ \case
  ReadTTY -> do
    c <- gets id
    case c of
      [] -> do
        new <- readTTY
        case splitter new of
          [] -> return mempty
          (x:xs) -> put xs >> return x
      (x:xs) -> put xs >> return x
  WriteTTY msg -> mapM_ writeTTY $ splitter msg

-- | Convert the type carried by `Teletype` to another type purely.
mapTeletype :: (b -> a)
            -> (a -> b)
            -> Sem (Teletype a ': r) c
            -> Sem (Teletype b ': r) c
mapTeletype fReads fWrites = reinterpret $ \case
  ReadTTY -> fReads <$> readTTY
  WriteTTY msg -> writeTTY $ fWrites msg

-- | Convert the type carried by `Teletype` to another type 
-- utilizing `Sem` monadic functions.
mapMTeletype :: (stringy -> Sem r strlike)
             -> (strlike -> Sem r stringy)
             -> Sem (Teletype stringy ': r) a
             -> Sem (Teletype strlike ': r) a
mapMTeletype fWrites fReads = reinterpret $ \case
  ReadTTY -> readTTY >>= raise . fReads
  WriteTTY msg -> raise (fWrites msg) >>= writeTTY

-- | Pass only reads, writes are replaced by `mempty`
teletypeReadOnly :: (Monoid s)
                 => Sem (Teletype s ': r) a
                 -> Sem (Teletype s ': r) a
teletypeReadOnly = mapTeletype id (pure mempty)

-- | Run `Teletype a` trying to parse elements utilizing attoparsec `Parser`
-- in streaming fashion
teletypeParseWith :: forall stringy a b r . (Eq stringy, Monoid stringy)
                  => (Parser stringy a -> stringy -> IResult stringy a)
                  -> (Parser stringy a)
                  -> (a -> stringy)
                  -> Sem (Teletype a ': r) b
                  -> Sem (Error String ': Teletype stringy ': r) b
teletypeParseWith parsefn parser builder =
    fmap snd
  . runState @stringy mempty
  . reinterpret3 \case

  ReadTTY -> do
    let lo :: IResult stringy a
           -> Sem (State stringy ': Error String : Teletype stringy : r) a
        lo (Done rest a)                   = put rest >> return a
        lo (Partial f)                     = f <$> readTTY >>= lo
        lo (Fail _unconsumed _context err) = throw err

    get @stringy >>= \case
      x | x /= mempty -> lo $ parsefn parser x
      _ | otherwise   -> parsefn parser <$> readTTY >>= lo

  WriteTTY msg -> writeTTY $ builder msg

