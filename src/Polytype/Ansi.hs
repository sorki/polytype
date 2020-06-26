{-|
Description : Functionality for stripping ANSI escape sequences from Teletypes

From https://hackage.haskell.org/package/strip-ansi-escape
* adjusted for streaming so it doesn't expect @endOfInput@
* fixed few unhandled sequences

XXX: submit PR

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Polytype.Ansi
  ( teletypeStripAnsi
  , skipEscapeSequence
  , isEsc
  , textEnv
  , ParserEnv (..)
  ) where

import           Control.Applicative            (optional, (<|>))
import           Control.Monad                  (void)
import           Data.Attoparsec.Internal.Types (ChunkElem)
import           Data.Attoparsec.Types          (Chunk, Parser)
import           Data.Char                      (isDigit)
import           Data.Text                      (Text)

import qualified Data.Attoparsec.Combinator     as AC
import qualified Data.Attoparsec.Text

import Polysemy
import Polysemy.Error

import Polytype.Teletype

-- | Strip ANSI escape sequences from Teletype input
teletypeStripAnsi :: Sem (Teletype Text : r) a
                  -> Sem (Error String : Teletype Text : r) a
teletypeStripAnsi =
  teletypeParseWith
    Data.Attoparsec.Text.parse
    (skipEscapeSequence textEnv
      *> Data.Attoparsec.Text.takeWhile (not . isEsc)
    )
    id

textEnv :: ParserEnv Char Text
textEnv = ParserEnv
  Data.Attoparsec.Text.skipWhile
  Data.Attoparsec.Text.takeWhile1

{-
-- From: https://github.com/chalk/ansi-regex/blob/166a0d5eddedacf0db7ccd7ee137b862ab1dae70/index.js
  [\x001B\x009B]
  [[\]()#;?]*
    (?:
      (?:
        (?:
          [a-zA-Z\d]*
          (?:
            ;[-a-zA-Z\d\/#&.:=?%@~_]*
          )*
        )?
        \x0007
      )
      |
      (?:
        (?:
          \d{1,4}
          (?:;\d{0,4})*
        )?
        [\dA-PR-TZcf-ntqry=><~]
      )
    )
-}

isEsc :: Char -> Bool
isEsc = (== '\x001B')

type ChunkParser str = (Chunk str, ChunkElem str ~ Char) => Parser str ()

esc :: ChunkParser str
esc = skip isEsc

skipEscapeSequence :: ParserEnv (ChunkElem str) str -> ChunkParser str
skipEscapeSequence e = AC.skipMany $ do
  esc
  beginsWithOpenSquareBracket
    <|> beginsWithClosingSquareBracket
    <|> beginsWithParenthesis
    <|> beginsWithHash
    <|> singleChar
    <|> beginsWithDigit

 where
  singleChar =
    skip (`elem` ("ABCDHIJKSTZ=>12<78HcNOME" :: String))

  beginsWithDigit = do
    skip (`elem` ("5036" :: String))
    skip (== 'n')

  beginsWithClosingSquareBracket = do
    skip (== ']')
    skipWhile e (/= '\x0007') <* skipAny

  beginsWithOpenSquareBracket = do
    skip (== '[')
    _ <- optional $ skip (`elem` ("?;" :: String))
    AC.skipMany $ do
      AC.skipMany1 digit
      AC.skipMany $ do
        skip (== ';')
        AC.skipMany1 digit
    skip isEndChar

   where
    isEndChar c =
      isDigit c
        || between 'A' 'P'
        || between 'R' 'T'
        || c == 'Z'
        || c == 'X' -- added by srk
        || c == 'c'
        || c == 'd'
        || between 'f' 'n'
        || c `elem` ("tqry=><~" :: String)
     where
      between x y = x <= c && c <= y

  beginsWithParenthesis = do
    skipElems "()"
    skipElems "AB012"

  beginsWithHash = do
    skip (== '#')
    skip (`elem` ("34568" :: String))

skipElems :: (Chunk str, ChunkElem str ~ Char) => String -> Parser str ()
skipElems s = skip (`elem` s)

data ParserEnv c str = ParserEnv
  { skipWhile  :: (c -> Bool) -> Parser str ()
  , takeWhile1 :: (c-> Bool) -> Parser str str
  }

{-# INLINE skip #-}
skip :: Chunk str => (ChunkElem str -> Bool) -> Parser str ()
skip = void . AC.satisfyElem


{-# INLINE skipAny #-}
skipAny :: Chunk str => Parser str ()
skipAny = skip (const True)


{-# INLINE digit #-}
digit :: (Chunk str, ChunkElem str ~ Char) => Parser str ()
digit = skip $ \c -> '0' <= c && c <= '9'
