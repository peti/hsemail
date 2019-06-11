{- |
   Module      :  Text.Parsec.Rfc2821
   Copyright   :  (c) 2007-2019 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module exports parser combinators for the grammar described in RFC2821,
   \"Simple Mail Transfer Protocol\", <http://www.faqs.org/rfcs/rfc2821.html>.
 -}

{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Rfc2821 where

import Text.Parsec.Rfc2234

import Control.Exception ( assert )
import Control.Monad.State
import Data.Char ( toLower )
import Data.List ( intercalate )
import Text.Parsec hiding (crlf)

-- Customize hlint ...
{-# ANN module "HLint: ignore Use camelCase" #-}

----------------------------------------------------------------------
-- * Data Types for ESMTP Commands
----------------------------------------------------------------------

-- | The 'smtpCmd' parser will create this data type from a string. Note that
-- /all/ command parsers expect their input to be terminated with 'crlf'.

data EsmtpCmd
  = Helo String
  | Ehlo String
  | MailFrom Mailbox            -- ^ Might be 'nullPath'.
  | RcptTo Mailbox              -- ^ Might be 'postmaster'.
  | Data
  | Rset
  | Send Mailbox
  | Soml Mailbox
  | Saml Mailbox
  | Vrfy String
  | Expn String
  | Help String                 -- ^ Might be @[]@.
  | Noop                        -- ^ Optional argument ignored.
  | Quit
  | Turn
  | WrongArg String ParseError
      -- ^ When a valid command has been recognized, but the
      -- argument parser fails, then this type will be
      -- returned. The 'String' contains the name of the
      -- command (in all upper-case) and the 'ParseError'
      -- is, obviously, the error description.
  deriving (Eq)

instance Show EsmtpCmd where
  show (Helo str)       = "HELO " ++ str
  show (Ehlo str)       = "EHLO " ++ str
  show (MailFrom mbox)  = "MAIL FROM:" ++ show mbox
  show (RcptTo mbox)    = "RCPT TO:" ++ show mbox
  show Data             = "DATA"
  show Rset             = "RSET"
  show (Send mbox)      = "SEND " ++ show mbox
  show (Soml mbox)      = "SOML " ++ show mbox
  show (Saml mbox)      = "SAML " ++ show mbox
  show (Vrfy str)       = "VRFY " ++ str
  show (Expn str)       = "EXPN " ++ str
  show Noop             = "NOOP"
  show Quit             = "QUIT"
  show Turn             = "TURN"
  show (Help t)
    | null t            = "HELP"
    | otherwise         = "HELP " ++ t
  show (WrongArg str _) = "Syntax error in argument of " ++ str ++ "."

-- | The most general e-mail address has the form:
-- @\<[\@route,...:]user\@domain\>@. This type, too, supports 'show' and
-- 'read'. Note that a \"shown\" address is /always/ enclosed in angular
-- brackets. When comparing two mailboxes for equality, the hostname is
-- case-insensitive.

data Mailbox = Mailbox [String] String String

instance Eq Mailbox where
  lhs == rhs  =  norm lhs == norm rhs
    where
    norm (Mailbox rt lp hp) = (rt, lp, map toLower hp)

instance Show Mailbox where
  show (Mailbox [] [] []) = "<>"
  show (Mailbox [] "postmaster" []) = "<postmaster>"
  show (Mailbox p u d) = "<" ++ route ++ (if null route then [] else ":") ++ mbox ++ ">"
    where
      route = intercalate "," . map ((:) '@') $ p
      mbox  = u ++ "@" ++ d

instance Read Mailbox where
  readsPrec _ = parsec2read (path <|> mailbox)
  readList    = error "reading [Mailbox] is not supported"

-- | @nullPath@ @=@ @'Mailbox' [] \"\" \"\" = \"\<\>\"@

nullPath :: Mailbox
nullPath = Mailbox [] [] []

-- | @postmaster@ @=@ @'Mailbox' [] \"postmaster\" \"\" = \"\<postmaster\>\"@

postmaster :: Mailbox
postmaster = Mailbox [] "postmaster" []


----------------------------------------------------------------------
-- * Data Types for ESMTP Replies
----------------------------------------------------------------------

-- | An ESMTP reply is a three-digit return code plus some waste of bandwidth
-- called \"comments\". This is what the list of strings is for; one string per
-- line in the reply. 'show' will append an \"@\\r\\n@\" end-of-line marker to
-- each entry in that list, so that the resulting string is ready to be sent
-- back to the peer. For example:
--
-- >>> show $ Reply (Code Success MailSystem 0) ["worked", "like", "a charm" ]
-- "250-worked\r\n250-like\r\n250 a charm\r\n"
--
-- If the message is an empty list @[]@, a default text will be constructed:
--
-- >>> show $ Reply (Code Success MailSystem 0) []
-- "250 Success in category MailSystem\r\n"

data EsmtpReply = Reply EsmtpCode [String]

data EsmtpCode = Code SuccessCode Category Int

data SuccessCode
  = Unused0
  | PreliminarySuccess
  | Success
  | IntermediateSuccess
  | TransientFailure
  | PermanentFailure
  deriving (Enum, Bounded, Eq, Ord, Show)

data Category
  = Syntax
  | Information
  | Connection
  | Unspecified3
  | Unspecified4
  | MailSystem
  deriving (Enum, Bounded, Eq, Ord, Show)

instance Show EsmtpReply where
  show (Reply c@(Code suc cat _) []) =
    let msg = show suc ++ " in category " ++ show cat
    in
    show $ Reply c [msg]

  show (Reply code msg) =
    let prefixCon = show code ++ "-"
        prefixEnd = show code ++ " "
        fmt p l   = p ++ l ++ "\r\n"
        (x:xs) = reverse msg
        msgCon = map (fmt prefixCon) xs
        msgEnd = fmt prefixEnd x
        msg'   = reverse (msgEnd:msgCon)
    in
    concat msg'

instance Show EsmtpCode where
  show (Code suc cat n) =
    assert (n >= 0 && n <= 9) $
      (show . fromEnum) suc ++ (show . fromEnum) cat ++ show n

-- | Construct a 'Reply'. Fails 'assert' if invalid numbers are given.

reply :: Int -> Int -> Int -> [String] -> EsmtpReply
reply suc c n msg =
  assert (suc >= 0 && suc <= 5) $
    assert (c >= 0 && c <= 5)   $
      assert (n >= 0 && n <= 9) $
        Reply (Code (toEnum suc) (toEnum c) n) msg

-- | A reply constitutes \"success\" if the status code is any of
-- 'PreliminarySuccess', 'Success', or 'IntermediateSuccess'.

isSuccess :: EsmtpReply -> Bool
isSuccess (Reply (Code PreliminarySuccess _ _) _)  = True
isSuccess (Reply (Code Success _ _) _)             = True
isSuccess (Reply (Code IntermediateSuccess _ _) _) = True
isSuccess _                                        = False

-- | A reply constitutes \"failure\" if the status code is either
-- 'PermanentFailure' or 'TransientFailure'.

isFailure :: EsmtpReply -> Bool
isFailure (Reply (Code PermanentFailure _ _) _) = True
isFailure (Reply (Code TransientFailure _ _) _) = True
isFailure _                                     = False

-- | The replies @221@ and @421@ signify 'Shutdown'.

isShutdown :: EsmtpReply -> Bool
isShutdown (Reply (Code Success Connection 1) _)          = True
isShutdown (Reply (Code TransientFailure Connection 1) _) = True
isShutdown _                                              = False

----------------------------------------------------------------------
-- * Command Parsers
----------------------------------------------------------------------

-- | This parser recognizes any of the ESMTP commands defined below. Note that
-- /all/ command parsers expect their input to be terminated with 'crlf'.

esmtpCmd :: Stream s m Char => ParsecT s u m EsmtpCmd

esmtpCmd = choice
           [ esmtpData, rset, noop, quit, turn
           , helo, mail, rcpt, send, soml, saml
           , vrfy, expn, help, ehlo
           ]

-- | The parser name \"data\" was taken.
esmtpData :: Stream s m Char => ParsecT s u m EsmtpCmd
rset, quit, turn, helo, ehlo, mail :: Stream s m Char => ParsecT s u m EsmtpCmd
rcpt, send, soml, saml, vrfy, expn :: Stream s m Char => ParsecT s u m EsmtpCmd
help                               :: Stream s m Char => ParsecT s u m EsmtpCmd

-- | May have an optional 'word' argument, but it is ignored.
noop :: Stream s m Char => ParsecT s u m EsmtpCmd

esmtpData = mkCmd0 "DATA" Data
rset = mkCmd0 "RSET" Rset
quit = mkCmd0 "QUIT" Quit
turn = mkCmd0 "TURN" Turn
helo = mkCmd1 "HELO" Helo     domain
ehlo = mkCmd1 "EHLO" Ehlo     domain
mail = mkCmd1 "MAIL" MailFrom from_path
rcpt = mkCmd1 "RCPT" RcptTo   to_path
send = mkCmd1 "SEND" Send     from_path
soml = mkCmd1 "SOML" Soml     from_path
saml = mkCmd1 "SAML" Saml     from_path
vrfy = mkCmd1 "VRFY" Vrfy     word
expn = mkCmd1 "EXPN" Expn     word

help = try (mkCmd0 "HELP" (Help [])) <|>
       mkCmd1 "HELP" Help (option [] word)

noop = try (mkCmd0 "NOOP" Noop) <|>
       mkCmd1 "NOOP" (const Noop) (option [] word)


----------------------------------------------------------------------
-- * Argument Parsers
----------------------------------------------------------------------

from_path :: Stream s m Char => ParsecT s u m Mailbox
from_path = do
  caseString "from:"
  (try (string "<>" >> return nullPath) <|> path)
                                <?> "from-path"

to_path :: Stream s m Char => ParsecT s u m Mailbox
to_path = do
  caseString "to:"
  (try (caseString "<postmaster>" >> return postmaster)
     <|> path)                  <?> "to-path"

path :: Stream s m Char => ParsecT s u m Mailbox
path = between (char '<') (char '>') (p <?> "path")
  where
  p = do
    r1 <- option [] (a_d_l >>= \r -> char ':' >> return r)
    (Mailbox _ l d) <- mailbox
    return (Mailbox r1 l d)

mailbox :: Stream s m Char => ParsecT s u m Mailbox
mailbox = (Mailbox [] <$> local_part <* char '@' <*> domain) <?> "mailbox"

local_part :: Stream s m Char => ParsecT s u m String
local_part = (dot_string <|> quoted_string) <?> "local-part"

domain :: Stream s m Char => ParsecT s u m String
domain = choice
         [ tokenList subdomain '.'  <?> "domain"
         , address_literal          <?> "address literal"
         ]

a_d_l :: Stream s m Char => ParsecT s u m [String]
a_d_l = sepBy1 at_domain (char ',') <?> "route-list"

at_domain :: Stream s m Char => ParsecT s u m String
at_domain = (char '@' >> domain) <?> "at-domain"

-- | /TODO/: Add IPv6 address and general literals
address_literal :: Stream s m Char => ParsecT s u m String
address_literal = ipv4_literal  <?> "IPv4 address literal"

ipv4_literal :: Stream s m Char => ParsecT s u m String
ipv4_literal = do
  rs <- between (char '[') (char ']') ipv4addr
  return ('[': reverse (']': reverse rs))

ipv4addr :: Stream s m Char => ParsecT s u m String
ipv4addr = p <?> "IPv4 address literal"
  where
  p = do
    r1 <- snum
    r2 <- char '.' >> snum
    r3 <- char '.' >> snum
    r4 <- char '.' >> snum
    return (r1 ++ "." ++ r2 ++ "." ++ r3 ++ "." ++ r4)

subdomain :: Stream s m Char => ParsecT s u m String
subdomain = p <?> "domain name"
  where
  p = do
    r <- many1 (alpha <|> digit <|> char '-')
    if last r == '-'
        then fail "subdomain must not end with hyphen"
        else return r

dot_string :: Stream s m Char => ParsecT s u m String
dot_string = tokenList atom '.' <?> "dot_string"

atom :: Stream s m Char => ParsecT s u m String
atom = many1 atext              <?> "atom"
  where
  atext = alpha <|> digit <|> oneOf "!#$%&'*+-/=?^_`{|}~"

snum :: Stream s m Char => ParsecT s u m String
snum = do
  r <- manyNtoM 1 3 digit
  if (read r :: Int) > 255
     then fail "IP address parts must be 0 <= x <= 255"
     else return r

number :: Stream s m Char => ParsecT s u m String
number = many1 digit

-- | This is a useful addition: The parser accepts an 'atom' or a
-- 'quoted_string'.

word :: Stream s m Char => ParsecT s u m String
word = (atom <|> fmap show quoted_string)
       <?> "word or quoted-string"


----------------------------------------------------------------------
-- * Helper Functions
----------------------------------------------------------------------

-- | Make the string 'crlf' terminated no matter what. \'@\\n@\' is expanded,
-- otherwise 'crlf' is appended. Note that if the string was terminated
-- incorrectly before, it still is. This function is useful when reading input
-- with 'System.IO.hGetLine' which removes the end-of-line delimiter.

{-# ANN fixCRLF "HLint: ignore Use list literal pattern" #-}
fixCRLF :: String -> String
fixCRLF ('\r' :'\n':[]) = fixCRLF []
fixCRLF (  x  :'\n':[]) = x : fixCRLF []
fixCRLF (  x  :  xs   ) = x : fixCRLF xs
fixCRLF      [ ]        = "\r\n"

-- | Construct a parser for a command without arguments. Expects 'crlf'!

mkCmd0 :: Stream s m Char => String -> a -> ParsecT s u m a
mkCmd0 str cons = (do
  try (caseString str)
  _ <- skipMany wsp >> crlf
  return cons)                          <?> str

-- | Construct a parser for a command with an argument, which the given parser
-- will handle. The result of the argument parser will be applied to the type
-- constructor before it is returned. Expects 'crlf'!
--
-- TODO: Commands that need an argument should diagnose a 'WrongArg' without
--       requiring a space, i.e. @EHLO@ should be recognized as missing a
--       required argument.

mkCmd1 :: Stream s m Char => String -> (a -> EsmtpCmd) -> ParsecT s u m a
       -> ParsecT s u m EsmtpCmd
mkCmd1 str cons p = do
  try (caseString str)
  _ <- wsp
  input <- getInput
  st <- getState
  let eol = skipMany wsp >> crlf
      p'  = between (many wsp) eol p <?> str
  r <- lift $ runParserT p' st "" input
  case r of
    Left e  -> return (WrongArg str e)
    Right a -> return (cons a)

-- | @tokenList p '.'@ will parse a token of the form \"@p.p@\", or
-- \"@p.p.p@\", and so on. Used in 'domain' and 'dot_string', for example.

tokenList :: Stream s m Char => ParsecT s u m String -> Char -> ParsecT s u m String
tokenList p c = fmap (intercalate [c]) (sepBy1 p (char c))
