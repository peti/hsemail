{- |
   Module      :  Text.Parsec.Rfc2234
   Copyright   :  (c) 2007-2019 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module provides parsers for the grammar defined in
   RFC2234, \"Augmented BNF for Syntax Specifications:
   ABNF\", <http://www.faqs.org/rfcs/rfc2234.html>. The
   terminal called @char@ in the RFC is called 'character'
   here to avoid conflicts with Parsec's 'char' function.
 -}

{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Rfc2234
  ( caseChar, caseString
  , manyN, manyNtoM
  , alpha, bit, character, cr, lf, crlf, ctl, dquote, hexdig
  , htab, lwsp, octet, sp, vchar, wsp
  , quoted_pair, quoted_string
  ) where

import Control.Monad ( liftM2, replicateM )
import Data.Char ( toUpper, chr, ord )
import Text.Parsec hiding ( crlf )

-- Customize hlint ...
{-# ANN module "HLint: ignore Use camelCase" #-}

----------------------------------------------------------------------
-- * Parser Combinators
----------------------------------------------------------------------

-- | Case-insensitive variant of Parsec's 'char' function.

caseChar :: Stream s m Char => Char -> ParsecT s u m Char
caseChar c = satisfy (\x -> toUpper x == toUpper c)

-- | Case-insensitive variant of Parsec's 'string' function.

caseString :: Stream s m Char => String -> ParsecT s u m ()
caseString cs = mapM_ caseChar cs <?> cs

-- | Match a parser at least @n@ times.

manyN :: Int -> ParsecT s u m a -> ParsecT s u m [a]
manyN n p | n <= 0    = return []
          | otherwise = liftM2 (++) (replicateM n p) (many p)

-- | Match a parser at least @n@ times, but no more than @m@ times.

manyNtoM :: Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
manyNtoM n m p
  | n < 0     = return []
  | n > m     = return []
  | n == m    = replicateM n p
  | n == 0    = foldr ((<|>) . (\x -> try (replicateM x p))) (return []) (reverse [1 .. m])
  | otherwise = liftM2 (++) (replicateM n p) (manyNtoM 0 (m - n) p)

----------------------------------------------------------------------
-- * Primitive Parsers
----------------------------------------------------------------------

-- | Match any character of the alphabet.

alpha :: Stream s m Char => ParsecT s u m Char
alpha = satisfy (\c -> c `elem` (['A' .. 'Z'] ++ ['a' .. 'z'])) <?> "alphabetic character"

-- | Match either \"1\" or \"0\".

bit :: Stream s m Char => ParsecT s u m Char
bit = oneOf "01" <?> "bit ('0' or '1')"

-- | Match any 7-bit US-ASCII character except for NUL (ASCII value 0, that
-- is).

character :: Stream s m Char => ParsecT s u m Char
character = satisfy (\c -> (c >= chr 1) && (c <= chr 127)) <?> "7-bit character excluding NUL"

-- | Match the carriage return character @\\r@.

cr :: Stream s m Char => ParsecT s u m Char
cr = char '\r' <?> "carriage return"

-- | Match returns the linefeed character @\\n@.

lf :: Stream s m Char => ParsecT s u m Char
lf = char '\n' <?> "linefeed"

-- | Match the Internet newline @\\r\\n@.

crlf :: Stream s m Char => ParsecT s u m String
crlf = do c <- cr
          l <- lf
          return [c, l]
       <?> "carriage return followed by linefeed"

-- | Match any US-ASCII control character. That is any character with a decimal
-- value in the range of [0..31,127].

ctl :: Stream s m Char => ParsecT s u m Char
ctl = satisfy (\c -> ord c `elem` ([0 .. 31] ++ [127])) <?> "control character"

-- | Match the double quote character \"@\"@\".

dquote :: Stream s m Char => ParsecT s u m Char
dquote = char (chr 34) <?> "double quote"

-- | Match any character that is valid in a hexadecimal number; [\'0\'..\'9\']
-- and [\'A\'..\'F\',\'a\'..\'f\'] that is.

hexdig :: Stream s m Char => ParsecT s u m Char
hexdig = hexDigit <?> "hexadecimal digit"

-- | Match the tab (\"@\\t@\") character.

htab :: Stream s m Char => ParsecT s u m Char
htab = char '\t' <?> "horizontal tab"

-- | Match \"linear white-space\". That is any number of consecutive 'wsp',
-- optionally followed by a 'crlf' and (at least) one more 'wsp'.

lwsp :: Stream s m Char => ParsecT s u m String
lwsp = do r  <- choice [many1 wsp, try (liftM2 (++) crlf (many1 wsp))]
          rs <- option [] lwsp
          return (r ++ rs)
       <?> "linear white-space"

-- | Match /any/ character.
octet :: Stream s m Char => ParsecT s u m Char
octet = anyChar <?> "any 8-bit character"

-- | Match the space.

sp :: Stream s m Char => ParsecT s u m Char
sp = char ' ' <?> "space"

-- | Match any printable ASCII character. (The \"v\" stands for \"visible\".)
-- That is any character in the decimal range of [33..126].

vchar :: Stream s m Char => ParsecT s u m Char
vchar = satisfy (\c -> (c >= chr 33) && (c <= chr 126)) <?> "printable character"

-- | Match either 'sp' or 'htab'.

wsp :: Stream s m Char => ParsecT s u m Char
wsp = sp <|> htab <?> "white-space"


-- ** Useful additions

-- | Match a \"quoted pair\". Any characters (excluding CR and LF) may be
-- quoted.

quoted_pair :: Stream s m Char => ParsecT s u m String
quoted_pair = do _ <- char '\\'
                 r <- noneOf "\r\n"
                 return ['\\', r]
              <?> "quoted pair"

-- | Match a quoted string. The specials \"@\\@\" and \"@\"@\" must be escaped
-- inside a quoted string; CR and LF are not allowed at all.

quoted_string :: Stream s m Char => ParsecT s u m String
quoted_string = do _ <- dquote
                   r <- many qcont
                   _ <- dquote
                   return ("\"" ++ concat r ++ "\"")
                <?> "quoted string"
 where
  qtext = noneOf "\\\"\r\n"
  qcont = many1 qtext <|> quoted_pair
