{- |
   Module      :  Text.Parsec.Rfc2822
   Copyright   :  (c) 2007-2019 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module provides parsers for the grammar defined in RFC2822,
   \"Internet Message Format\", <http://www.faqs.org/rfcs/rfc2822.html>.
-}

{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Rfc2822 where

import Text.Parsec.Rfc2234 hiding ( quoted_pair, quoted_string )

import Control.Monad ( replicateM, guard )
import Data.Char ( ord )
import Data.Functor
import Data.List ( intercalate )
import Data.Maybe ( catMaybes )
import Data.Monoid ( Monoid, mempty )
import Data.Time.Calendar.Compat
import Data.Time.LocalTime
import Text.Parsec hiding ( crlf )

-- Customize hlint ...
{-# ANN module "HLint: ignore Use camelCase" #-}

-- * Useful parser combinators

-- |Return @Nothing@ if the given parser doesn't match. This
-- combinator is included in the latest parsec distribution as
-- @optionMaybe@, but ghc-6.6.1 apparently doesn't have it.

maybeOption    :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Maybe a)
maybeOption p   = option Nothing (fmap Just p)

-- |@unfold@ @=@ @between (optional cfws) (optional cfws)@

unfold          :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
unfold           = between (optional cfws) (optional cfws)

-- |Construct a parser for a message header line from the
-- header's name and a parser for the body.

header          :: Stream s m Char => String -> ParsecT s u m a -> ParsecT s u m a
header n p       = let nameString = caseString (n ++ ":")
                   in
                   between nameString crlf p <?> (n ++ " header line")

-- |Like 'header', but allows the obsolete white-space rules.

obs_header      :: Stream s m Char => String -> ParsecT s u m a -> ParsecT s u m a
obs_header n p   = let nameString = caseString n >> many wsp >> char ':'
                   in
                   between nameString crlf p <?> ("obsolete " ++ n ++ " header line")


-- ** Primitive Tokens (section 3.2.1)

-- |Match any US-ASCII non-whitespace control character.

no_ws_ctl       :: Stream s m Char => ParsecT s u m Char
no_ws_ctl       = satisfy (\c -> ord c `elem` ([1..8] ++ [11,12] ++ [14..31] ++ [127]))
                  <?> "US-ASCII non-whitespace control character"

-- |Match any US-ASCII character except for @\r@, @\n@.

text            :: Stream s m Char => ParsecT s u m Char
text            = satisfy (\c -> ord c `elem` ([1..9] ++ [11,12] ++ [14..127]))
                  <?> "US-ASCII character (excluding CR and LF)"

-- |Match any of the RFC's \"special\" characters: @()\<\>[]:;\@,.\\\"@.

specials        :: Stream s m Char => ParsecT s u m Char
specials        = oneOf "()<>[]:;@,.\\\""   <?> "one of ()<>[]:;@,.\\\""


-- ** Quoted characters (section 3.2.2)

-- |Match a \"quoted pair\". All characters matched by 'text' may be
-- quoted. Note that the parsers returns /both/ characters, the
-- backslash and the actual content.

quoted_pair     :: Stream s m Char => ParsecT s u m String
quoted_pair     = try obs_qp <|> do { _ <- char '\\'; r <- text; return ['\\',r] }
                  <?> "quoted pair"

-- ** Folding white space and comments (section 3.2.3)

-- |Match \"folding whitespace\". That is any combination of 'wsp' and
-- 'crlf' followed by 'wsp'.

fws             :: Stream s m Char => ParsecT s u m String
fws             = do r <- many1 $ choice [ blanks, linebreak]
                     return (concat r)
    where
    blanks      = many1 wsp
    linebreak   = try $ do { r1 <- crlf; r2 <- blanks; return (r1 ++ r2) }

-- |Match any non-whitespace, non-control character except for \"@(@\",
-- \"@)@\", and \"@\\@\". This is used to describe the legal content of
-- 'comment's.
--
-- /Note/: This parser accepts 8-bit characters, even though this is
-- not legal according to the RFC. Unfortunately, 8-bit content in
-- comments has become fairly common in the real world, so we'll just
-- accept the fact.

ctext           :: Stream s m Char => ParsecT s u m Char
ctext           = no_ws_ctl <|> satisfy (\c -> ord c `elem` ([33..39] ++ [42..91] ++ [93..126] ++ [128..255]))
                  <?> "any regular character (excluding '(', ')', and '\\')"

-- |Match a \"comments\". That is any combination of 'ctext',
-- 'quoted_pair's, and 'fws' between brackets. Comments may nest.

comment         :: Stream s m Char => ParsecT s u m String
comment         = do _ <- char '('
                     r1 <- many ccontent
                     r2 <- option [] fws
                     _ <- char ')'
                     return ("(" ++ concat r1 ++ r2 ++ ")")
                  <?> "comment"
    where
    ccontent    = try $ do r1 <- option [] fws
                           r2 <- choice [many1 ctext, quoted_pair, comment]
                           return (r1 ++ r2)

-- |Match any combination of 'fws' and 'comments'.

cfws            :: Stream s m Char => ParsecT s u m String
cfws            = do r <- many1 $ choice [ fws, comment ]
                     return (concat r)

-- ** Atom (section 3.2.4)

-- |Match any US-ASCII character except for control characters,
-- 'specials', or space. 'atom' and 'dot_atom' are made up of this.

atext           :: Stream s m Char => ParsecT s u m Char
atext           = alpha <|> digit <|> oneOf "!#$%&'*+-/=?^_`{|}~"
                  <?> "US-ASCII character (excluding controls, space, and specials)"

-- |Match one or more 'atext' characters and skip any preceding or
-- trailing 'cfws'.

atom            :: Stream s m Char => ParsecT s u m String
atom            = unfold (many1 atext <?> "atom")

-- |Match 'dot_atom_text' and skip any preceding or trailing 'cfws'.

dot_atom        :: Stream s m Char => ParsecT s u m String
dot_atom        = unfold (dot_atom_text <?> "dot atom")

-- |Match two or more 'atext's interspersed by dots.

dot_atom_text   :: Stream s m Char => ParsecT s u m String
dot_atom_text   = fmap (intercalate ".") (sepBy1 (many1 atext) (char '.'))
                  <?> "dot atom content"


-- ** Quoted strings (section 3.2.5)

-- |Match any non-whitespace, non-control US-ASCII character except
-- for \"@\\@\" and \"@\"@\".

qtext           :: Stream s m Char => ParsecT s u m Char
qtext           = no_ws_ctl <|> satisfy (\c -> ord c `elem` ([33] ++ [35..91] ++ [93..126]))
                  <?> "US-ASCII character (excluding '\\', and '\"')"

-- |Match either 'qtext' or 'quoted_pair'.

qcontent        :: Stream s m Char => ParsecT s u m String
qcontent        = many1 qtext <|> quoted_pair
                  <?> "quoted string content"

-- |Match any number of 'qcontent' between double quotes. Any 'cfws'
-- preceding or following the \"atom\" is skipped automatically.

quoted_string   :: Stream s m Char => ParsecT s u m String
quoted_string   = unfold (do _ <- dquote
                             r1 <- many (do r1 <- option [] fws
                                            r2 <- qcontent
                                            return (r1 ++ r2))
                             r2 <- option [] fws
                             _ <- dquote
                             return ("\"" ++ concat r1 ++ r2 ++ "\""))
                  <?> "quoted string"


-- * Miscellaneous tokens (section 3.2.6)

-- |Match either 'atom' or 'quoted_string'.

word            :: Stream s m Char => ParsecT s u m String
word            = unfold (atom <|> quoted_string)     <?> "word"

-- |Match either one or more 'word's or an 'obs_phrase'.

phrase          :: Stream s m Char => ParsecT s u m [String]
phrase          = {- many1 word <?> "phrase" <|> -} obs_phrase

-- |Match any non-whitespace, non-control US-ASCII character except
-- for \"@\\@\" and \"@\"@\".

utext           :: Stream s m Char => ParsecT s u m Char
utext           = no_ws_ctl <|> satisfy (\c -> ord c `elem` [33..126])
                  <?> "regular US-ASCII character (excluding '\\', and '\"')"

-- |Match any number of 'utext' tokens.
--
-- \"Unstructured text\" is used in free text fields such as 'subject'.
-- Please note that any comments or whitespace that prefaces or
-- follows the actual 'utext' is /included/ in the returned string.

unstructured    :: Stream s m Char => ParsecT s u m String
unstructured    = do r1 <- option [] fws
                     r2 <- many (do r3 <- utext
                                    r4 <- option [] fws
                                    return (r3 : r4))
                     return (r1 ++ concat r2)
                  <?> "unstructured text"


-- * Date and Time Specification (section 3.3)

-- |Parse a date and time specification of the form
--
-- >   Thu, 19 Dec 2002 20:35:46 +0200
--
-- where the weekday specification \"@Thu,@\" is optional. The parser
-- returns an appropriate 'ZonedTime'
--
-- TODO: Nor will the 'date_time' parser perform /any/ consistency checking. It
-- will accept
--
-- >>> parseTest date_time "Wed, 30 Apr 2002 13:12 +0100"
-- 2002-04-30 13:12:00 +0100

date_time       :: Stream s m Char => ParsecT s u m ZonedTime
date_time       = do optional (try (day_of_week >> char ','))
                     d <- date
                     _ <- fws
                     (td,z) <- time
                     optional cfws
                     return (ZonedTime (LocalTime d td) z)
                  <?> "date/time specification"

-- |This parser matches a 'day_name' or an 'obs_day_of_week' (optionally
-- wrapped in folding whitespace) and return the appropriate 'DayOfWeek' value.

day_of_week     :: Stream s m Char => ParsecT s u m DayOfWeek
day_of_week     =     try (between (optional fws) (optional fws) day_name <?> "name of a day-of-the-week")
                  <|> obs_day_of_week

-- |This parser recognizes abbreviated weekday names (\"@Mon@\",
-- \"@Tue@\",...).

day_name        :: Stream s m Char => ParsecT s u m DayOfWeek
day_name        =     caseString "Mon" $> Monday
                  <|> try (caseString "Tue" $> Tuesday)
                  <|> caseString "Wed" $> Wednesday
                  <|> caseString "Thu" $> Thursday
                  <|> caseString "Fri" $> Friday
                  <|> try (caseString "Sat" $> Saturday)
                  <|> caseString "Sun" $> Sunday
                  <?> "name of a day-of-the-week"

-- |This parser will match a date of the form \"@dd:mm:yyyy@\" and return
-- a tripple of the form (Int,Month,Int) - corresponding to
-- (year,month,day).

date :: Stream s m Char => ParsecT s u m Day
date = do d <- day
          m <- month
          y <- year
          return (fromGregorian (fromIntegral y) m d)
       <?> "date specification"

-- |This parser will match a four digit number and return its integer
-- value. No range checking is performed.

year :: Stream s m Char => ParsecT s u m Int
year = do y <- manyN 4 digit
          return (read y :: Int)
       <?> "year"

-- |This parser will match a 'month_name', optionally wrapped in
-- folding whitespace, or an 'obs_month' and return its 'Month'
-- value.

month :: Stream s m Char => ParsecT s u m Int
month =     try (between (optional fws) (optional fws) month_name <?> "month name")
        <|> obs_month


-- |This parser will the abbreviated month names (\"@Jan@\", \"@Feb@\", ...)
-- and return the appropriate 'Int' value in the range of (1,12).

month_name :: Stream s m Char => ParsecT s u m Int
month_name =     (try (caseString "Jan") $> 1)
             <|> (caseString "Feb" $> 2)
             <|> (try (caseString "Mar") $> 3)
             <|> (try (caseString "Apr") $> 4)
             <|> (caseString "May" $>  5)
             <|> (try (caseString "Jun") $>  6)
             <|> (caseString "Jul" $> 7)
             <|> (caseString "Aug" $> 8)
             <|> (caseString "Sep" $> 9)
             <|> (caseString "Oct" $> 10)
             <|> (caseString "Nov" $> 11)
             <|> (caseString "Dec" $> 12)
             <?> "month name"

-- Internal helper function: match a 1 or 2-digit number (day of month).

day_of_month :: Stream s m Char => ParsecT s u m Int
day_of_month = do r <- fmap read (manyNtoM 1 2 digit)
                  guard (r >= 1 && r < 31)
                  return r

-- |Match a 1 or 2-digit number (day of month), recognizing both
-- standard and obsolete folding syntax.

day :: Stream s m Char => ParsecT s u m Int
day = try obs_day <|> day_of_month <?> "day"

-- |This parser will match a 'time_of_day' specification followed by a
-- 'zone'. It returns the tuple (TimeOfDay,Int) corresponding to the
-- return values of either parser.

time :: Stream s m Char => ParsecT s u m (TimeOfDay, TimeZone)
time = do t <- time_of_day
          _ <- fws
          z <- zone
          return (t,z)
       <?> "time and zone specification"

-- |This parser will match a time-of-day specification of \"@hh:mm@\" or
-- \"@hh:mm:ss@\" and return the corrsponding time as a 'TimeOfDay'.
--
-- >>> parseTest (time_of_day <* eof) "12:03:23"
-- 12:03:23
-- >>> parseTest (time_of_day <* eof) "99:99:99"
-- parse error at (line 1, column 3):unknown parse error

time_of_day :: Stream s m Char => ParsecT s u m TimeOfDay
time_of_day = do h <- hour
                 _ <- char ':'
                 m <- minute
                 s <- option 0 (do { _ <- char ':'; second } )
                 return (TimeOfDay h m (fromIntegral s))
              <?> "time specification"

-- |This parser matches a two-digit number in the range (0,24) and returns its
-- integer value.
--
-- >>> parseTest hour "034"
-- 3
-- >>> parseTest hour "99"
-- parse error at (line 1, column 3):unknown parse error

hour :: Stream s m Char => ParsecT s u m Int
hour = do r <- fmap read (replicateM 2 digit)
          guard (r >= 0 && r <= 24)
          return r
       <?> "hour"

-- |This parser will match a two-digit number in the range (0,60) and return
-- its integer value.
--
-- >>> parseTest minute "34"
-- 34
-- >>> parseTest minute "61"
-- parse error at (line 1, column 3):unknown parse error
-- >>> parseTest (minute <* eof) "034"
-- parse error at (line 1, column 3):
-- unexpected '4'
-- expecting end of input

minute :: Stream s m Char => ParsecT s u m Int
minute = do r <- fmap read (replicateM 2 digit)
            guard (r >= 0 && r <= 60)
            return r
         <?> "minute"

-- |This parser will match a two-digit number in the range (0,60) and return
-- its integer value.
--
-- >>> parseTest second "34"
-- 34

second :: Stream s m Char => ParsecT s u m Int
second = minute <?> "second"

-- |This parser will match a timezone specification of the form
-- \"@+hhmm@\" or \"@-hhmm@\" and return the zone's offset to UTC in
-- seconds as an integer. 'obs_zone' is matched as well.

zone :: Stream s m Char => ParsecT s u m TimeZone
zone = do sign <- choice [char '+' $> 1, char '-' $> (-1) ]
          h <- hour
          m <- minute
          return (minutesToTimeZone (sign*((h*60)+m)))
       <|> obs_zone

-- * Address Specification (section 3.4)

-- |A NameAddr is composed of an optional realname a mandatory
-- e-mail 'address'.

data NameAddr = NameAddr { nameAddr_name :: Maybe String
                         , nameAddr_addr :: String
                         }
                deriving (Show,Eq)

-- |Parse a single 'mailbox' or an address 'group' and return the
-- address(es).

address         :: Stream s m Char => ParsecT s u m [NameAddr]
address         = try (do { r <- mailbox; return [r] }) <|> group
                  <?> "address"

-- |Parse a 'name_addr' or an 'addr_spec' and return the
-- address.

mailbox         :: Stream s m Char => ParsecT s u m NameAddr
mailbox         = try name_addr <|> fmap (NameAddr Nothing) addr_spec
                  <?> "mailbox"

-- |Parse an 'angle_addr', optionally prefaced with a 'display_name',
-- and return the address.

name_addr       :: Stream s m Char => ParsecT s u m NameAddr
name_addr       = (NameAddr <$> maybeOption display_name <*> angle_addr) <?> "name address"


-- |Parse an 'angle_addr' or an 'obs_angle_addr' and return the address.

angle_addr      :: Stream s m Char => ParsecT s u m String
angle_addr      = try (unfold (do _ <- char '<'
                                  r <- addr_spec
                                  _ <- char '>'
                                  return r)
                       <?> "angle address"
                      )
                  <|> obs_angle_addr

-- |Parse a \"group\" of addresses. That is a 'display_name', followed
-- by a colon, optionally followed by a 'mailbox_list', followed by a
-- semicolon. The found address(es) are returned - what may be none.
-- Here is an example:
--
-- >>> parse group "" "my group: user1@example.org, user2@example.org;"
-- Right [NameAddr {nameAddr_name = Nothing, nameAddr_addr = "user1@example.org"},NameAddr {nameAddr_name = Nothing, nameAddr_addr = "user2@example.org"}]

group           :: Stream s m Char => ParsecT s u m [NameAddr]
group           = do _ <- display_name
                     _ <- char ':'
                     r <- option [] mailbox_list
                     _ <- unfold $ char ';'
                     return r
                  <?> "address group"

-- |Parse and return a 'phrase'.

display_name    :: Stream s m Char => ParsecT s u m String
display_name    = fmap unwords phrase
                  <?> "display name"

-- |Parse a list of 'mailbox' addresses, every two addresses being
-- separated by a comma, and return the list of found address(es).

mailbox_list    :: Stream s m Char => ParsecT s u m [NameAddr]
mailbox_list    = sepBy mailbox (char ',') <?> "mailbox list"

-- |Parse a list of 'address' addresses, every two addresses being
-- separated by a comma, and return the list of found address(es).

address_list    :: Stream s m Char => ParsecT s u m [NameAddr]
address_list    = do { r <-sepBy address (char ','); return (concat r) }
                  <?> "address list"


-- ** Addr-spec specification (section 3.4.1)

-- |Parse an \"address specification\". That is a 'local_part', followed
-- by an \"@\@@\" character, followed by a 'domain'. Return the complete
-- address as 'String', ignoring any whitespace or any comments.

addr_spec       :: Stream s m Char => ParsecT s u m String
addr_spec       = do r1 <- local_part
                     _ <- char '@'
                     r2 <- domain
                     return (r1 ++ "@" ++ r2)
                  <?> "address specification"

-- |Parse and return a \"local part\" of an 'addr_spec'. That is either
-- a 'dot_atom' or a 'quoted_string'.

local_part      :: Stream s m Char => ParsecT s u m String
local_part      = try obs_local_part <|> dot_atom <|> quoted_string
                  <?> "address' local part"

-- |Parse and return a \"domain part\" of an 'addr_spec'. That is either
-- a 'dot_atom' or a 'domain_literal'.

domain          :: Stream s m Char => ParsecT s u m String
domain          = try obs_domain <|> dot_atom <|> domain_literal
                  <?> "address' domain part"

-- |Parse a \"domain literal\". That is a \"@[@\" character, followed by
-- any amount of 'dcontent', followed by a terminating \"@]@\"
-- character. The complete string is returned verbatim.

domain_literal  :: Stream s m Char => ParsecT s u m String
domain_literal  = unfold (do _ <- char '['
                             r <- many (optional fws >> dcontent)
                             optional fws
                             _ <- char ']'
                             return ("[" ++ concat r ++ "]"))
                  <?> "domain literal"

-- |Parse and return any characters that are legal in a
-- 'domain_literal'. That is 'dtext' or a 'quoted_pair'.

dcontent        :: Stream s m Char => ParsecT s u m String
dcontent        = many1 dtext <|> quoted_pair
                  <?> "domain literal content"

-- |Parse and return any ASCII characters except \"@[@\", \"@]@\", and
-- \"@\\@\".

dtext           :: Stream s m Char => ParsecT s u m Char
dtext           = no_ws_ctl
                  <|> satisfy (\c -> ord c `elem` ([33..90] ++ [94..126]))
                  <?> "any ASCII character (excluding '[', ']', and '\\')"


-- * Overall message syntax (section 3.5)

-- |This data type represents a parsed Internet Message as defined in
-- this RFC. It consists of an arbitrary number of header lines,
-- represented in the 'Field' data type, and a message body, which may
-- be empty.

data GenericMessage a = Message [Field] a deriving Show

-- |Parse a complete message as defined by this RFC and it broken down
-- into the separate header fields and the message body. Header lines,
-- which contain syntax errors, will not cause the parser to abort.
-- Rather, these headers will appear as 'OptionalField's (which are
-- unparsed) in the resulting 'Message'. A message must be really,
-- really badly broken for this parser to fail.
--
-- This behaviour was chosen because it is impossible to predict what
-- the user of this module considers to be a fatal error;
-- traditionally, parsers are very forgiving when it comes to Internet
-- messages.
--
-- If you want to implement a really strict parser, you'll have to put
-- the appropriate parser together yourself. You'll find that this is
-- rather easy to do. Refer to the 'fields' parser for further details.

message         :: (Monoid s, Stream s m Char) => ParsecT s u m (GenericMessage s)
message         = do f <- fields
                     b <- option mempty (do _ <- crlf; body)
                     return (Message f b)

-- |A message body is just an unstructured sequence of characters.

body            :: (Monoid s, Monad m) => ParsecT s u m s
body            = do v <- getInput
                     setInput mempty
                     return v

-- * Field definitions (section 3.6)

-- |This data type represents any of the header fields defined in this
-- RFC. Each of the various instances contains with the return value
-- of the corresponding parser.

data Field      = OptionalField       String String
                | From                [NameAddr]
                | Sender              NameAddr
                | ReturnPath          String
                | ReplyTo             [NameAddr]
                | To                  [NameAddr]
                | Cc                  [NameAddr]
                | Bcc                 [NameAddr]
                | MessageID           String
                | InReplyTo           [String]
                | References          [String]
                | Subject             String
                | Comments            String
                | Keywords            [[String]]
                | Date                ZonedTime
                | ResentDate          ZonedTime
                | ResentFrom          [NameAddr]
                | ResentSender        NameAddr
                | ResentTo            [NameAddr]
                | ResentCc            [NameAddr]
                | ResentBcc           [NameAddr]
                | ResentMessageID     String
                | ResentReplyTo       [NameAddr]
                | Received            ([(String,String)], ZonedTime)
                | ObsReceived         [(String,String)]
                deriving (Show)

-- |This parser will parse an arbitrary number of header fields as
-- defined in this RFC. For each field, an appropriate 'Field' value
-- is created, all of them making up the 'Field' list that this parser
-- returns.
--
-- If you look at the implementation of this parser, you will find
-- that it uses Parsec's 'try' modifier around /all/ of the fields.
-- The idea behind this is that fields, which contain syntax errors,
-- fall back to the catch-all 'optional_field'. Thus, this parser will
-- hardly ever return a syntax error -- what conforms with the idea
-- that any message that can possibly be accepted /should/ be.

fields          :: Stream s m Char => ParsecT s u m [Field]
fields          = many $ choice
                         [ try (From <$> from)
                         , try (Sender <$> sender)
                         , try (ReturnPath <$> return_path)
                         , try (ReplyTo <$> reply_to)
                         , try (To <$> to)
                         , try (Cc <$> cc)
                         , try (Bcc <$> bcc)
                         , try (MessageID <$> message_id)
                         , try (InReplyTo <$> in_reply_to)
                         , try (References <$> references)
                         , try (Subject <$> subject)
                         , try (Comments <$> comments)
                         , try (Keywords <$> keywords)
                         , try (Date <$> orig_date)
                         , try (ResentDate <$> resent_date)
                         , try (ResentFrom <$> resent_from)
                         , try (ResentSender <$> resent_sender)
                         , try (ResentTo <$> resent_to)
                         , try (ResentCc <$> resent_cc)
                         , try (ResentBcc <$> resent_bcc)
                         , try (ResentMessageID <$> resent_msg_id)
                         , try (Received <$> received)
                         , uncurry OptionalField <$> optional_field  -- catch all
                         ]

-- ** The origination date field (section 3.6.1)

-- |Parse a \"@Date:@\" header line and return the date it contains a
-- 'CalendarTime'.

orig_date       :: Stream s m Char => ParsecT s u m ZonedTime
orig_date       = header "Date" date_time


-- ** Originator fields (section 3.6.2)

-- |Parse a \"@From:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

from            :: Stream s m Char => ParsecT s u m [NameAddr]
from            = header "From" mailbox_list

-- |Parse a \"@Sender:@\" header line and return the 'mailbox' address
-- contained in it.

sender          :: Stream s m Char => ParsecT s u m NameAddr
sender          = header "Sender" mailbox

-- |Parse a \"@Reply-To:@\" header line and return the 'address_list'
-- address(es) contained in it.

reply_to        :: Stream s m Char => ParsecT s u m [NameAddr]
reply_to        = header "Reply-To" address_list


-- ** Destination address fields (section 3.6.3)

-- |Parse a \"@To:@\" header line and return the 'address_list'
-- address(es) contained in it.

to              :: Stream s m Char => ParsecT s u m [NameAddr]
to              = header "To" address_list

-- |Parse a \"@Cc:@\" header line and return the 'address_list'
-- address(es) contained in it.

cc              :: Stream s m Char => ParsecT s u m [NameAddr]
cc              = header "Cc" address_list

-- |Parse a \"@Bcc:@\" header line and return the 'address_list'
-- address(es) contained in it.

bcc             :: Stream s m Char => ParsecT s u m [NameAddr]
bcc             = header "Bcc" (try address_list <|> do { optional cfws; return [] })

-- ** Identification fields (section 3.6.4)

-- |Parse a \"@Message-Id:@\" header line and return the 'msg_id'
-- contained in it.

message_id      :: Stream s m Char => ParsecT s u m String
message_id      = header "Message-ID" msg_id

-- |Parse a \"@In-Reply-To:@\" header line and return the list of
-- 'msg_id's contained in it.

in_reply_to     :: Stream s m Char => ParsecT s u m [String]
in_reply_to     = header "In-Reply-To" (many1 msg_id)

-- |Parse a \"@References:@\" header line and return the list of
-- 'msg_id's contained in it.

references      :: Stream s m Char => ParsecT s u m [String]
references      = header "References" (many1 msg_id)

-- |Parse a \"@message ID:@\" and return it. A message ID is almost
-- identical to an 'angle_addr', but with stricter rules about folding
-- and whitespace.

msg_id          :: Stream s m Char => ParsecT s u m String
msg_id          = unfold (do _ <- char '<'
                             idl <- id_left
                             _ <- char '@'
                             idr <- id_right
                             _ <- char '>'
                             return ("<" ++ idl ++ "@" ++ idr ++ ">"))
                  <?> "message ID"

-- |Parse a \"left ID\" part of a 'msg_id'. This is almost identical to
-- the 'local_part' of an e-mail address, but with stricter rules
-- about folding and whitespace.

id_left         :: Stream s m Char => ParsecT s u m String
id_left         = dot_atom_text <|> no_fold_quote
                  <?> "left part of an message ID"

-- |Parse a \"right ID\" part of a 'msg_id'. This is almost identical to
-- the 'domain' of an e-mail address, but with stricter rules about
-- folding and whitespace.

id_right        :: Stream s m Char => ParsecT s u m String
id_right        = dot_atom_text <|> no_fold_literal
                  <?> "right part of an message ID"

-- |Parse one or more occurrences of 'qtext' or 'quoted_pair' and
-- return the concatenated string. This makes up the 'id_left' of a
-- 'msg_id'.

no_fold_quote   :: Stream s m Char => ParsecT s u m String
no_fold_quote   = do _ <- dquote
                     r <- many (many1 qtext <|> quoted_pair)
                     _ <- dquote
                     return ("\"" ++ concat r ++ "\"")
                  <?> "non-folding quoted string"

-- |Parse one or more occurrences of 'dtext' or 'quoted_pair' and
-- return the concatenated string. This makes up the 'id_right' of a
-- 'msg_id'.

no_fold_literal :: Stream s m Char => ParsecT s u m String
no_fold_literal = do _ <- char '['
                     r <- many (many1 dtext <|> quoted_pair)
                     _ <- char ']'
                     return ("[" ++ concat r ++ "]")
                  <?> "non-folding domain literal"


-- ** Informational fields (section 3.6.5)

-- |Parse a \"@Subject:@\" header line and return its contents verbatim.
-- Please note that all whitespace and/or comments are preserved, i.e.
-- the result of parsing @\"Subject: foo\"@ is @\" foo\"@, not @\"foo\"@.

subject         :: Stream s m Char => ParsecT s u m String
subject         = header "Subject" unstructured

-- |Parse a \"@Comments:@\" header line and return its contents verbatim.
-- Please note that all whitespace and/or comments are preserved, i.e.
-- the result of parsing @\"Comments: foo\"@ is @\" foo\"@, not @\"foo\"@.

comments        :: Stream s m Char => ParsecT s u m String
comments        = header "Comments" unstructured

-- |Parse a \"@Keywords:@\" header line and return the list of 'phrase's
-- found. Please not that each phrase is again a list of 'atom's, as
-- returned by the 'phrase' parser.

keywords        :: Stream s m Char => ParsecT s u m [[String]]
keywords        = header "Keywords" (do r1 <- phrase
                                        r2 <- many (do _ <- char ','; phrase)
                                        return (r1:r2))


-- ** Resent fields (section 3.6.6)

-- |Parse a \"@Resent-Date:@\" header line and return the date it
-- contains as 'ZonedTime'.

resent_date     :: Stream s m Char => ParsecT s u m ZonedTime
resent_date     = header "Resent-Date" date_time

-- |Parse a \"@Resent-From:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

resent_from     :: Stream s m Char => ParsecT s u m [NameAddr]
resent_from     = header "Resent-From" mailbox_list


-- |Parse a \"@Resent-Sender:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

resent_sender   :: Stream s m Char => ParsecT s u m NameAddr
resent_sender   = header "Resent-Sender" mailbox


-- |Parse a \"@Resent-To:@\" header line and return the 'mailbox'
-- address contained in it.

resent_to       :: Stream s m Char => ParsecT s u m [NameAddr]
resent_to       = header "Resent-To" address_list

-- |Parse a \"@Resent-Cc:@\" header line and return the 'address_list'
-- address(es) contained in it.

resent_cc       :: Stream s m Char => ParsecT s u m [NameAddr]
resent_cc       = header "Resent-Cc" address_list

-- |Parse a \"@Resent-Bcc:@\" header line and return the 'address_list'
-- address(es) contained in it. (This list may be empty.)

resent_bcc      :: Stream s m Char => ParsecT s u m [NameAddr]
resent_bcc      = header "Resent-Bcc" (    try address_list
                                       <|> do optional cfws
                                              return []
                                      )
                  <?> "Resent-Bcc: header line"

-- |Parse a \"@Resent-Message-ID:@\" header line and return the 'msg_id'
-- contained in it.

resent_msg_id   :: Stream s m Char => ParsecT s u m String
resent_msg_id   = header "Resent-Message-ID" msg_id


-- ** Trace fields (section 3.6.7)

return_path     :: Stream s m Char => ParsecT s u m String
return_path     = header "Return-Path" path

path            :: Stream s m Char => ParsecT s u m String
path            = unfold ( try (do _ <- char '<'
                                   r <- option "" addr_spec
                                   _ <- char '>'
                                   return ("<" ++ r ++ ">")
                               )
                          <|> obs_path
                         )
                  <?> "return path spec"

received        :: Stream s m Char => ParsecT s u m ([(String,String)], ZonedTime)
received        = header "Received" (do r1 <- name_val_list
                                        _ <- char ';'
                                        r2 <- date_time
                                        return (r1,r2))

name_val_list   :: Stream s m Char => ParsecT s u m [(String,String)]
name_val_list   = do optional cfws
                     many1 name_val_pair
                  <?> "list of name/value pairs"

name_val_pair   :: Stream s m Char => ParsecT s u m (String,String)
name_val_pair   = do r1 <- item_name
                     _ <- cfws
                     r2 <- item_value
                     return (r1,r2)
                  <?> "a name/value pair"

item_name       :: Stream s m Char => ParsecT s u m String
item_name       = do r1 <- alpha
                     r2 <- many $ choice [ char '-', alpha, digit ]
                     return (r1 : r2)
                  <?> "name of a name/value pair"

item_value      :: Stream s m Char => ParsecT s u m String
item_value      = choice [ try (do { r <- many1 angle_addr; return (concat r) })
                         , try addr_spec
                         , try domain
                         , msg_id
                         , try atom
                         ]
                  <?> "value of a name/value pair"

-- ** Optional fields (section 3.6.8)

-- |Parse an arbitrary header field and return a tuple containing the
-- 'field_name' and 'unstructured' text of the header. The name will
-- /not/ contain the terminating colon.

{-# ANN optional_field "HLint: ignore Reduce duplication" #-}

optional_field  :: Stream s m Char => ParsecT s u m (String,String)
optional_field  = do n <- field_name
                     _ <- char ':'
                     b <- unstructured
                     _ <- crlf
                     return (n,b)
                  <?> "optional (unspecified) header line"

-- |Parse and return an arbitrary header field name. That is one or
-- more 'ftext' characters.

field_name      :: Stream s m Char => ParsecT s u m String
field_name      = many1 ftext <?> "header line name"

-- |Match and return any ASCII character except for control
-- characters, whitespace, and \"@:@\".

ftext           :: Stream s m Char => ParsecT s u m Char
ftext           = satisfy (\c -> ord c `elem` ([33..57] ++ [59..126]))
                  <?> "character (excluding controls, space, and ':')"


-- * Miscellaneous obsolete tokens (section 4.1)

-- |Match the obsolete \"quoted pair\" syntax, which - unlike
-- 'quoted_pair' - allowed /any/ ASCII character to be specified when
-- quoted. The parser will return both, the backslash and the actual
-- character.

obs_qp          :: Stream s m Char => ParsecT s u m String
obs_qp          = do _ <- char '\\'
                     c <- satisfy (\c -> ord c `elem` [0..127])
                     return ['\\',c]
                  <?> "any quoted US-ASCII character"

-- |Match the obsolete \"text\" syntax, which - unlike 'text' - allowed
-- \"carriage returns\" and \"linefeeds\". This is really weird; you
-- better consult the RFC for details. The parser will return the
-- complete string, including those special characters.

obs_text        :: Stream s m Char => ParsecT s u m String
obs_text        = do r1 <- many lf
                     r2 <- many cr
                     r3 <- many (do r4 <- obs_char
                                    r5 <- many lf
                                    r6 <- many cr
                                    return (r4 : (r5 ++ r6)))
                     return (r1 ++ r2 ++ concat r3)

-- |Match and return the obsolete \"char\" syntax, which - unlike
-- 'character' - did not allow \"carriage return\" and \"linefeed\".

obs_char        :: Stream s m Char => ParsecT s u m Char
obs_char        = satisfy (\c -> ord c `elem` ([0..9] ++ [11,12] ++ [14..127]))
                  <?> "any ASCII character except CR and LF"

-- |Match and return the obsolete \"utext\" syntax, which is identical
-- to 'obs_text'.

obs_utext       :: Stream s m Char => ParsecT s u m String
obs_utext       = obs_text

-- |Match the obsolete \"phrase\" syntax, which - unlike 'phrase' -
-- allows dots between tokens.

obs_phrase      :: Stream s m Char => ParsecT s u m [String]
obs_phrase      = do r1 <- word
                     r2 <- many $ choice [ word
                                         , string "."
                                         , do { _ <- cfws; return [] }
                                         ]
                     return (r1 : filter (/=[]) r2)

-- |Match a  \"phrase list\" syntax and return the list of 'String's
-- that make up the phrase. In contrast to a 'phrase', the
-- 'obs_phrase_list' separates the individual words by commas. This
-- syntax is - as you will have guessed - obsolete.

obs_phrase_list :: Stream s m Char => ParsecT s u m [String]
obs_phrase_list = do r1 <- many1 (do r <- option [] phrase
                                     _ <- unfold $ char ','
                                     return (filter (/=[]) r))
                     r2 <- option [] phrase
                     return (concat r1 ++ r2)
                  <|> phrase


-- * Obsolete folding white space (section 4.2)

-- |Parse and return an \"obsolete fws\" token. That is at least one
-- 'wsp' character, followed by an arbitrary number (including zero)
-- of 'crlf' followed by at least one more 'wsp' character.

obs_fws         :: Stream s m Char => ParsecT s u m String
obs_fws         = do r1 <- many1 wsp
                     r2 <- many (do r3 <- crlf
                                    r4 <- many1 wsp
                                    return (r3 ++ r4))
                     return (r1 ++ concat r2)


-- * Obsolete Date and Time (section 4.3)

-- |Parse a 'day_name' but allow for the obsolete folding syntax.
-- TODO

obs_day_of_week :: Stream s m Char => ParsecT s u m DayOfWeek
obs_day_of_week = unfold day_name <?> "day-of-the-week name"

-- |Parse a 'year' but allow for a two-digit number (obsolete) and the
-- obsolete folding syntax.

obs_year        :: Stream s m Char => ParsecT s u m Int
obs_year        = unfold (do r <- manyN 2 digit
                             return (normalize (read r :: Int)))
                  <?> "year"
    where
    normalize n
        | n <= 49   = 2000 + n
        | n <= 999  = 1900 + n
        | otherwise = n

-- |Parse a 'month_name' but allow for the obsolete folding syntax.

obs_month       :: Stream s m Char => ParsecT s u m Int
obs_month       = between cfws cfws month_name <?> "month name"

-- |Parse a 'day' but allow for the obsolete folding syntax.

obs_day         :: Stream s m Char => ParsecT s u m Int
obs_day         = unfold day_of_month <?> "day"

-- |Parse a 'hour' but allow for the obsolete folding syntax.

obs_hour        :: Stream s m Char => ParsecT s u m Int
obs_hour        = unfold hour <?> "hour"

-- |Parse a 'minute' but allow for the obsolete folding syntax.

obs_minute      :: Stream s m Char => ParsecT s u m Int
obs_minute      = unfold minute <?> "minute"

-- |Parse a 'second' but allow for the obsolete folding syntax.

obs_second      :: Stream s m Char => ParsecT s u m Int
obs_second      = unfold second <?> "second"

-- |Match the obsolete zone names and return the appropriate offset.

obs_zone :: Stream s m Char => ParsecT s u m TimeZone
obs_zone = choice [ parseZone "UT"  0
                  , parseZone "GMT" 0
                  , parseZone "EST" (-5)
                  , parseZone "EDT" (-4)
                  , parseZone "CST" (-6)
                  , parseZone "CDT" (-5)
                  , parseZone "MST" (-7)
                  , parseZone "MDT" (-6)
                  , parseZone "PST" (-8)
                  , parseZone "PDT" (-7)
                  , do { r <- oneOf ['A'..'I']; mkZone (ord r - 64) }    <?> "military zone spec"
                  , do { r <- oneOf ['K'..'M']; mkZone (ord r - 65) }    <?> "military zone spec"
                  , do { r <- oneOf ['N'..'Y']; mkZone (-(ord r - 77)) } <?> "military zone spec"
                  , parseZone "Z" 0                                      <?> "military zone spec"
                  ]
  where parseZone n o  = try (string n *> mkZone o)
        mkZone         = pure . hoursToTimeZone

-- * Obsolete Addressing (section 4.4)

-- |This parser matches the \"obsolete angle address\" syntax, a construct that
-- used to be called \"route address\" in earlier RFCs. It differs from a
-- standard 'angle_addr' in two ways: (1) it allows far more
-- liberal insertion of folding whitespace and comments and (2) the address may
-- contain a \"route\" (which this parser ignores):
--
-- >>> parse obs_angle_addr "" "<@example1.org,@example2.org:joe@example.org>"
-- Right "<joe@example.org>"

obs_angle_addr  :: Stream s m Char => ParsecT s u m String
obs_angle_addr  = unfold (do _ <- char '<'
                             _ <- option [] obs_route
                             addr <- addr_spec
                             _ <- char '>'
                             return ("<" ++ addr ++ ">") -- TODO: route is lost here.
                         )
                  <?> "obsolete angle address"

-- |This parser parses the \"route\" part of 'obs_angle_addr' and
-- returns the list of 'String's that make up this route. Relies on
-- 'obs_domain_list' for the actual parsing.

obs_route       :: Stream s m Char => ParsecT s u m [String]
obs_route       = unfold (do { r <- obs_domain_list; _ <- char ':'; return r })
                  <?> "route of an obsolete angle address"

-- |This parser parses a list of domain names, each of them prefaced
-- with an \"at\". Multiple names are separated by a comma. The list of
-- 'domain's is returned - and may be empty.

obs_domain_list :: Stream s m Char => ParsecT s u m [String]
obs_domain_list = do _ <- char '@'
                     r1 <- domain
                     r2 <- many (do _ <- cfws <|> string ","
                                    optional cfws
                                    _ <- char '@'
                                    domain)
                     return (r1 : r2)
                    <?> "route of an obsolete angle address"

-- |Parse the obsolete syntax of a 'local_part', which allowed for
-- more liberal insertion of folding whitespace and comments. The
-- actual string is returned.

obs_local_part  :: Stream s m Char => ParsecT s u m String
obs_local_part  = do r1 <- word
                     r2 <- many (do _ <- string "."
                                    r <- word
                                    return ('.' : r))
                     return (r1 ++ concat r2)
                  <?> "local part of an address"

-- |Parse the obsolete syntax of a 'domain', which allowed for more
-- liberal insertion of folding whitespace and comments. The actual
-- string is returned.

obs_domain      :: Stream s m Char => ParsecT s u m String
obs_domain      = do r1 <- atom
                     r2 <- many (do _ <- string "."
                                    r <- atom
                                    return ('.' : r))
                     return (r1 ++ concat r2)
                  <?> "domain part of an address"

-- |This parser will match the obsolete syntax for a 'mailbox_list'.
-- This one is quite weird: An 'obs_mbox_list' contains an arbitrary
-- number of 'mailbox'es - including none -, which are separated by
-- commas. But you may have multiple consecutive commas without giving
-- a 'mailbox'. You may also have a valid 'obs_mbox_list' that
-- contains /no/ 'mailbox' at all. On the other hand, you /must/ have
-- at least one comma. The following example is valid:
--
-- >>> parse obs_mbox_list "" ","
-- Right []
--
-- But this one is not:
--
-- >>> parse obs_mbox_list "" "joe@example.org"
-- Left (line 1, column 16):
-- unexpected end of input
-- expecting obsolete syntax for a list of mailboxes

obs_mbox_list   :: Stream s m Char => ParsecT s u m [NameAddr]
obs_mbox_list   = do r1 <- many1 (try (do r <- maybeOption mailbox
                                          _ <- unfold $ char ','
                                          return r))
                     r2 <- maybeOption mailbox
                     return (catMaybes (r1 ++ [r2]))
                  <?> "obsolete syntax for a list of mailboxes"

-- |This parser is identical to 'obs_mbox_list' but parses a list of
-- 'address'es rather than 'mailbox'es. The main difference is that an
-- 'address' may contain 'group's. Please note that as of now, the
-- parser will return a simple list of addresses; the grouping
-- information is lost.

obs_addr_list   :: Stream s m Char => ParsecT s u m [NameAddr]
obs_addr_list   = do r1 <- many1 (try (do r <- maybeOption address
                                          optional cfws
                                          _ <- char ','
                                          optional cfws
                                          return r))
                     r2 <- maybeOption address
                     return (concat (catMaybes (r1 ++ [r2])))
                  <?> "obsolete syntax for a list of addresses"


-- * Obsolete header fields (section 4.5)

obs_fields      :: Stream s m Char => ParsecT s u m [Field]
obs_fields      = many $ choice
                         [ try (From <$> obs_from)
                         , try (Sender <$> obs_sender)
                         , try (ReturnPath <$> obs_return)
                         , try (ReplyTo <$> obs_reply_to)
                         , try (To <$> obs_to)
                         , try (Cc <$> obs_cc)
                         , try (Bcc <$> obs_bcc)
                         , try (MessageID <$> obs_message_id)
                         , try (InReplyTo <$> obs_in_reply_to)
                         , try (References <$> obs_references)
                         , try (Subject <$> obs_subject)
                         , try (Comments <$> obs_comments)
                         , try (Keywords . return <$> obs_keywords)
                         , try (Date <$> obs_orig_date)
                         , try (ResentDate <$> obs_resent_date)
                         , try (ResentFrom <$> obs_resent_from)
                         , try (ResentSender <$> obs_resent_send)
                         , try (ResentTo <$> obs_resent_to)
                         , try (ResentCc <$> obs_resent_cc)
                         , try (ResentBcc <$> obs_resent_bcc)
                         , try (ResentMessageID <$> obs_resent_mid)
                         , try (ResentReplyTo <$> obs_resent_reply)
                         , try (ObsReceived <$> obs_received)
                         , uncurry OptionalField <$> obs_optional    -- catch all
                         ]
-- ** Obsolete origination date field (section 4.5.1)

-- |Parse a 'date' header line but allow for the obsolete
-- folding syntax.

obs_orig_date   :: Stream s m Char => ParsecT s u m ZonedTime
obs_orig_date   = obs_header "Date" date_time


-- ** Obsolete originator fields (section 4.5.2)

-- |Parse a 'from' header line but allow for the obsolete
-- folding syntax.

obs_from        :: Stream s m Char => ParsecT s u m [NameAddr]
obs_from        = obs_header "From" mailbox_list

-- |Parse a 'sender' header line but allow for the obsolete
-- folding syntax.

obs_sender      :: Stream s m Char => ParsecT s u m NameAddr
obs_sender      = obs_header "Sender" mailbox

-- |Parse a 'reply_to' header line but allow for the obsolete
-- folding syntax.

obs_reply_to    :: Stream s m Char => ParsecT s u m [NameAddr]
obs_reply_to    = obs_header "Reply-To" mailbox_list


-- ** Obsolete destination address fields (section 4.5.3)

-- |Parse a 'to' header line but allow for the obsolete
-- folding syntax.

obs_to          :: Stream s m Char => ParsecT s u m [NameAddr]
obs_to          = obs_header "To" address_list

-- |Parse a 'cc' header line but allow for the obsolete
-- folding syntax.

obs_cc          :: Stream s m Char => ParsecT s u m [NameAddr]
obs_cc          = obs_header "Cc" address_list

-- |Parse a 'bcc' header line but allow for the obsolete
-- folding syntax.

obs_bcc         :: Stream s m Char => ParsecT s u m [NameAddr]
obs_bcc         = header "Bcc" (    try address_list
                                    <|> do { optional cfws; return [] }
                               )


-- ** Obsolete identification fields (section 4.5.4)

-- |Parse a 'message_id' header line but allow for the obsolete
-- folding syntax.

obs_message_id  :: Stream s m Char => ParsecT s u m String
obs_message_id  = obs_header "Message-ID" msg_id

-- |Parse an 'in_reply_to' header line but allow for the obsolete
-- folding and the obsolete phrase syntax.

obs_in_reply_to :: Stream s m Char => ParsecT s u m [String]
obs_in_reply_to = obs_header "In-Reply-To" (do r <- many (    do {_ <- phrase; return [] }
                                                          <|> msg_id
                                                         )
                                               return (filter (/=[]) r))

-- |Parse a 'references' header line but allow for the obsolete
-- folding and the obsolete phrase syntax.

obs_references  :: Stream s m Char => ParsecT s u m [String]
obs_references  = obs_header "References" (do r <- many (    do { _ <- phrase; return [] }
                                                         <|> msg_id
                                                        )
                                              return (filter (/=[]) r))

-- |Parses the \"left part\" of a message ID, but allows the obsolete
-- syntax, which is identical to a 'local_part'.

obs_id_left     :: Stream s m Char => ParsecT s u m String
obs_id_left     = local_part <?> "left part of an message ID"

-- |Parses the \"right part\" of a message ID, but allows the obsolete
-- syntax, which is identical to a 'domain'.

obs_id_right    :: Stream s m Char => ParsecT s u m String
obs_id_right    = domain <?> "right part of an message ID"



-- ** Obsolete informational fields (section 4.5.5)

-- |Parse a 'subject' header line but allow for the obsolete
-- folding syntax.

obs_subject     :: Stream s m Char => ParsecT s u m String
obs_subject     = obs_header "Subject" unstructured

-- |Parse a 'comments' header line but allow for the obsolete
-- folding syntax.

obs_comments    :: Stream s m Char => ParsecT s u m String
obs_comments    = obs_header "Comments" unstructured

-- |Parse a 'keywords' header line but allow for the obsolete
-- folding syntax. Also, this parser accepts 'obs_phrase_list'.

obs_keywords    :: Stream s m Char => ParsecT s u m [String]
obs_keywords    = obs_header "Keywords" obs_phrase_list


-- ** Obsolete resent fields (section 4.5.6)

-- |Parse a 'resent_from' header line but allow for the obsolete
-- folding syntax.

obs_resent_from :: Stream s m Char => ParsecT s u m [NameAddr]
obs_resent_from = obs_header "Resent-From" mailbox_list

-- |Parse a 'resent_sender' header line but allow for the obsolete
-- folding syntax.

obs_resent_send :: Stream s m Char => ParsecT s u m NameAddr
obs_resent_send = obs_header "Resent-Sender" mailbox

-- |Parse a 'resent_date' header line but allow for the obsolete
-- folding syntax.

obs_resent_date :: Stream s m Char => ParsecT s u m ZonedTime
obs_resent_date = obs_header "Resent-Date" date_time

-- |Parse a 'resent_to' header line but allow for the obsolete
-- folding syntax.

obs_resent_to   :: Stream s m Char => ParsecT s u m [NameAddr]
obs_resent_to   = obs_header "Resent-To" mailbox_list

-- |Parse a 'resent_cc' header line but allow for the obsolete
-- folding syntax.

obs_resent_cc   :: Stream s m Char => ParsecT s u m [NameAddr]
obs_resent_cc   = obs_header "Resent-Cc" mailbox_list

-- |Parse a 'resent_bcc' header line but allow for the obsolete
-- folding syntax.

obs_resent_bcc  :: Stream s m Char => ParsecT s u m [NameAddr]
obs_resent_bcc  = obs_header "Bcc" (    try address_list
                                    <|> do { optional cfws; return [] }
                                   )

-- |Parse a 'resent_msg_id' header line but allow for the obsolete
-- folding syntax.

obs_resent_mid  :: Stream s m Char => ParsecT s u m String
obs_resent_mid  = obs_header "Resent-Message-ID" msg_id

-- |Parse a @Resent-Reply-To@ header line but allow for the
-- obsolete folding syntax.

obs_resent_reply :: Stream s m Char => ParsecT s u m [NameAddr]
obs_resent_reply = obs_header "Resent-Reply-To" address_list


-- ** Obsolete trace fields (section 4.5.7)

obs_return      :: Stream s m Char => ParsecT s u m String
obs_return       = obs_header "Return-Path" path

obs_received    :: Stream s m Char => ParsecT s u m [(String, String)]
obs_received     = obs_header "Received" name_val_list

-- |Match 'obs_angle_addr'.

obs_path        :: Stream s m Char => ParsecT s u m String
obs_path        = obs_angle_addr

-- |This parser is identical to 'optional_field' but allows the more
-- liberal line-folding syntax between the \"field_name\" and the \"field
-- text\".

obs_optional    :: Stream s m Char => ParsecT s u m (String,String)
obs_optional    = do n <- field_name
                     _ <- many wsp
                     _ <- char ':'
                     b <- unstructured
                     _ <- crlf
                     return (n,b)
                  <?> "optional (unspecified) header line"
