module Main ( main ) where

import Text.Parsec.Rfc2822

import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec
import Text.Parsec ( parse, eof )
import Text.Parsec.String ( Parser )

parseTest :: Parser a -> String -> IO a
parseTest p input = case parse (p <* eof) (show input) input of
                      Left err -> fail ("parse error at " ++ show err)
                      Right r -> return r

parseIdemTest :: Parser String -> String -> Expectation
parseIdemTest p input = parseTest p input `shouldReturn` input

parseFailure :: (Show a) => Parser a -> String -> Expectation
parseFailure p input = parse (do { r <- p; eof; return r }) (show input) input `shouldSatisfy` failure
  where
    failure (Left _) = True
    failure _        = False

main :: IO ()
main = hspec $ do
  describe "Rfc2822.quoted_pair" $
    it "can quote a nul byte" $
      parseIdemTest quoted_pair "\\\0"

  describe "Rfc2822.date_time" $
    it "parses hand-picked times correctly" $
      fmap zonedTimeToUTC (parseTest date_time "Fri, 21 Dec 2012 00:07:43 +0300") `shouldReturn`
        zonedTimeToUTC (ZonedTime (LocalTime (fromGregorian 2012 12 21) (TimeOfDay 0 7 43)) (hoursToTimeZone 3))

  describe "Rfc2822.day" $ do
    it "parses a hand-picked day-of-months correctly" $ do
      parseTest day "1" `shouldReturn` 1
      parseTest day "09" `shouldReturn` 9
      parseTest day "15" `shouldReturn` 15
      parseTest day "31" `shouldReturn` 31

    it "does perform range checking" $ do
      parseFailure day "00"
      parseFailure day "32"

    it "fails properly on incomplete input" $ do
      parseFailure day "Mon"
      parseFailure day "Thu"

  describe "Rfc2822.obs_mbox_list" $ do
    it "parses hand-picked inputs correctly" $ do
      parseTest obs_mbox_list "," `shouldReturn` []
      parseTest obs_mbox_list "Joe Doe <joe@example.org>,( \r\n bla),,jane@\r\n example.net \r\n (Jane Doe)," `shouldReturn`
        [NameAddr (Just "Joe Doe") "joe@example.org",NameAddr Nothing "jane@example.net"]

    it "fails properly on incomplete input" $
      parseFailure obs_mbox_list "foo@example.org"

  describe "Rfc2822.subject" $
    it "doesn't consume leading whitespace" $
      parseTest subject "Subject: foo\r\n" `shouldReturn` " foo"

  describe "Rfc2822.comment" $
    it "doesn't consume leading whitespace" $
      parseTest comments "Comments: foo\r\n" `shouldReturn` " foo"

  -- Most of the following test cases have been adapted from
  -- <http://hackage.haskell.org/package/email-validate>.
  describe "Rfc2822.addr_spec" $
    it "parses hand-picked inputs correctly" $ do
      parseFailure addr_spec "()[]\\;:,><@example.com" -- Disallowed Characters
      parseFailure addr_spec " -- test --@example.com" -- No spaces allowed in local part
      parseFailure addr_spec "-@..com"
      parseFailure addr_spec "-@a..com"
      parseFailure addr_spec ".@"
      parseFailure addr_spec ".@example.com" -- Phil Haack says so
      parseFailure addr_spec ".dot@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec ".first.last@example.com" -- Local part starts with a dot
      parseFailure addr_spec ".test@example.com"
      parseFailure addr_spec ".wooly@example.com" -- Phil Haack says so
      parseFailure addr_spec "@@bar.com"
      parseFailure addr_spec "@NotAnEmail" -- Phil Haack says so
      parseFailure addr_spec "@bar.com"
      parseFailure addr_spec "@example.com" -- No local part
      parseFailure addr_spec "Abc\\@def@example.com" -- Was incorrectly given as a valid address in the original RFC3696
      parseFailure addr_spec "Doug\\ \\\"Ace\\\"\\ L\\.@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "Doug\\ \\\"Ace\\\"\\ Lovell@example.com" -- Escaping can only happen in a quoted string
      parseFailure addr_spec "Fred\\ Bloggs@example.com" -- Was incorrectly given as a valid address in the original RFC3696
      parseFailure addr_spec "Ima Fool@example.com" -- Phil Haack says so
      parseFailure addr_spec "Invalid \\\n Folding \\\n Whitespace@example.com" -- This isn't FWS so Dominic Sayers says it's invalid
      parseFailure addr_spec "Joe.\\\\Blow@example.com" -- Was incorrectly given as a valid address in the original RFC3696
      parseFailure addr_spec "NotAnEmail" -- Phil Haack says so
      parseFailure addr_spec "[test]@example.com" -- Square brackets only allowed within quotes
      parseFailure addr_spec "\"Doug \"Ace\" L.\"@example.com" -- Doug Lovell says this should fail
      parseIdemTest addr_spec "\"\"@example.com"
      parseFailure addr_spec "\"\"\"@example.com" -- Local part contains unescaped excluded characters
      parseFailure addr_spec "\"\\\"@example.com" -- Local part cannot end with a backslash
      parseFailure addr_spec "\"first\"last\"@example.com" -- Local part contains unescaped excluded characters
      parseFailure addr_spec "\"first\\\\\"last\"@example.com" -- Contains an unescaped quote
      parseFailure addr_spec "\"foo\"(yay)@(hoopla)[1.2.3.4]" -- Address literal can't be commented (RFC5321)
      parseFailure addr_spec "\"null \NUL\"@char.com" -- cannot have unescaped null character
      parseFailure addr_spec "\"qu@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "\"test\"blah\"@example.com" -- Phil Haack says so
      parseFailure addr_spec "\"test\"test\"@example.com" -- Quotes cannot be nested
      parseFailure addr_spec "\"test\\\r\n blah\"@example.com" -- Folding white space can't appear within a quoted pair
      parseFailure addr_spec "\"test\rblah\"@example.com" -- Quoted string specifically excludes carriage returns
      parseFailure addr_spec "a(a(b(c)d(e(f))g)(h(i)j)@example.com" -- Braces are not properly matched
      parseFailure addr_spec "a@bar.com."
      parseFailure addr_spec "aaa.com"
      parseFailure addr_spec "aaa@.123"
      parseFailure addr_spec "aaa@.com"
      parseFailure addr_spec "aaa@[123.123.123.123]a" -- extra data outside ip
      parseFailure addr_spec "abc@def@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "abc\\@def@example.com" -- This example from RFC3696 was corrected in an erratum
      parseFailure addr_spec "abc\\@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "abc\\\\@def@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "abc\\\\@example.com" -- This example from RFC3696 was corrected in an erratum
      parseFailure addr_spec "cal(foo(bar)@iamcal.com" -- Unclosed parenthesis in comment
      parseFailure addr_spec "cal(foo)bar)@iamcal.com" -- Too many closing parentheses
      parseFailure addr_spec "cal(foo\\)@iamcal.com" -- Backslash at end of comment has nothing to escape
      parseFailure addr_spec "dot.@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "doug@" -- Doug Lovell says this should fail
      parseFailure addr_spec "first(12345678901234567890123456789012345678901234567890)last@(1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)example.com" -- Too long with comments, not too long without
      parseFailure addr_spec "first(abc(\"def\".ghi).mno)middle(abc(\"def\".ghi).mno).last@(abc(\"def\".ghi).mno)example(abc(\"def\".ghi).mno).(abc(\"def\".ghi).mno)com(abc(\"def\".ghi).mno)" -- Can't have comments or white space except at an element boundary
      parseFailure addr_spec "first(middle)last@example.com" -- Can't have a comment or white space except at an element boundary
      parseFailure addr_spec "first..last@example.com" -- Local part has consecutive dots
      parseFailure addr_spec "first.last" -- No @
      parseFailure addr_spec "first.last.@example.com" -- Local part ends with a dot
      parseFailure addr_spec "first.last@" -- No domain
      parseFailure addr_spec "first\\@last@example.com" -- Escaping can only happen within a quoted string
      parseFailure addr_spec "first\\\\@last@example.com" -- Local part contains unescaped excluded characters
      parseFailure addr_spec "first\\last@example.com" -- Unquoted string must be an atom
      parseFailure addr_spec "gatsby@f.sc.ot.t.f.i.tzg.era.l.d." -- Doug Lovell says this should fail
      parseFailure addr_spec "hello world@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "ote\"@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "phil.h\\@\\@ck@haacked.com" -- Escaping can only happen in a quoted string
      parseFailure addr_spec "pootietang.@example.com" -- Phil Haack says so
      parseFailure addr_spec "test..test@example.com"
      parseFailure addr_spec "test.@example.com"
      parseFailure addr_spec "test.\r\n\r\n obs@syntax.com" -- obs-fws must have at least one WSP per line
      parseFailure addr_spec "test.example.com"
      parseFailure addr_spec "test@." -- Dave Child says so
      parseFailure addr_spec "test@...........com" -- ......
      parseFailure addr_spec "test@.org" -- Dave Child says so
      parseFailure addr_spec "test@123.123.123.123]" -- Dave Child says so
      parseFailure addr_spec "test@@example.com"
      parseFailure addr_spec "test@[123.123.123.123" -- Dave Child says so
      parseFailure addr_spec "test@example." -- Dave Child says so
      parseFailure addr_spec "test@test@example.com"
      parseFailure addr_spec "two..dot@example.com" -- Doug Lovell says this should fail
      parseFailure addr_spec "wo..oly@example.com" -- Phil Haack says so
      parseFailure addr_spec "{^c\\@**Dog^}@cartoon.com" -- This is a throwaway example from Doug Lovell's article. Actually it's not a valid address.
      parseTest addr_spec " \r\n (\r\n x \r\n ) \r\n first\r\n ( \r\n x\r\n ) \r\n .\r\n ( \r\n x) \r\n last \r\n (  x \r\n ) \r\n @example.com" `shouldReturn` "first.last@example.com"
      parseIdemTest addr_spec "!def!xyz%abc@example.com"
      parseIdemTest addr_spec "$A12345@example.com"
      parseTest addr_spec "(foo)cal(bar)@(baz)iamcal.com(quux)" `shouldReturn` "cal@iamcal.com"
      parseIdemTest addr_spec "+1~1+@example.com"
      parseIdemTest addr_spec "+@b.c" -- TLDs can be any length
      parseIdemTest addr_spec "+@b.com"
      parseTest addr_spec "1234   @   local(blah)  .machine .example" `shouldReturn` "1234@local.machine.example"
      parseIdemTest addr_spec "1234567890123456789012345678901234567890123456789012345678901234@example.com"
      parseIdemTest addr_spec "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.123456789012345678901234567890123456789012345678901234567890123.example.com"
      parseIdemTest addr_spec "1234567890@example.com"
      parseTest addr_spec "HM2Kinsists@(that comments are allowed)this.is.ok" `shouldReturn` "HM2Kinsists@this.is.ok"
      parseIdemTest addr_spec "Ima.Fool@example.com"
      parseIdemTest addr_spec "TEST@example.com"
      parseTest addr_spec "Test.\r\n Folding.\r\n Whitespace@example.com" `shouldReturn` "Test.Folding.Whitespace@example.com"
      parseIdemTest addr_spec "\"Abc@def\"@example.com"
      parseIdemTest addr_spec "\"Abc\\@def\"@example.com"
      parseIdemTest addr_spec "\"Austin@Powers\"@example.com"
      parseIdemTest addr_spec "\"Doug \\\"Ace\\\" L.\"@example.com"
      parseIdemTest addr_spec "\"Fred Bloggs\"@example.com"
      parseIdemTest addr_spec "\"Fred\\ Bloggs\"@example.com"
      parseIdemTest addr_spec "\"Ima Fool\"@example.com"
      parseIdemTest addr_spec "\"Ima.Fool\"@example.com"
      parseIdemTest addr_spec "\"Joe.\\\\Blow\"@example.com"
      parseIdemTest addr_spec "\"Joe\\\\Blow\"@example.com"
      parseIdemTest addr_spec "\"Test \\\"Fail\\\" Ing\"@example.com"
      parseIdemTest addr_spec "\"[[ test ]]\"@example.com"
      parseIdemTest addr_spec "\"first last\"@example.com"
      parseIdemTest addr_spec "\"first(last)\"@example.com"
      parseIdemTest addr_spec "\"first..last\"@example.com" -- obs-local-part form as described in RFC 2822
      parseIdemTest addr_spec "\"first.middle.last\"@example.com" -- obs-local-part form as described in RFC 2822
      parseIdemTest addr_spec "\"first.middle\".\"last\"@example.com" -- obs-local-part form as described in RFC 2822
      parseIdemTest addr_spec "\"first@last\"@example.com"
      parseIdemTest addr_spec "\"first\".\"last\"@example.com"
      parseIdemTest addr_spec "\"first\".\"middle\".\"last\"@example.com" -- obs-local-part form as described in RFC 2822
      parseIdemTest addr_spec "\"first\".last@example.com" -- obs-local-part form as described in RFC 2822
      parseIdemTest addr_spec "\"first\".middle.\"last\"@example.com"
      parseIdemTest addr_spec "\"first\\\"last\"@example.com"
      parseIdemTest addr_spec "\"first\\\\\\\"last\"@example.com"
      parseIdemTest addr_spec "\"first\\\\last\"@example.com"
      parseIdemTest addr_spec "\"first\\last\"@example.com" -- Any character can be escaped in a quoted string
      parseIdemTest addr_spec "\"hello my name is\"@stutter.com"
      parseIdemTest addr_spec "\"null \\\NUL\"@char.com" -- can have escaped null character
      parseIdemTest addr_spec "\"test.test\"@example.com"
      parseIdemTest addr_spec "\"test@test\"@example.com"
      parseIdemTest addr_spec "\"test\\\"blah\"@example.com"
      parseIdemTest addr_spec "\"test\\\\blah\"@example.com"
      parseIdemTest addr_spec "\"test\\\rblah\"@example.com" -- Quoted string specifically excludes carriage returns unless escaped
      parseIdemTest addr_spec "\"test\\blah\"@example.com" -- Any character can be escaped in a quoted string
      parseIdemTest addr_spec "\"test\\test\"@example.com" -- Any character can be escaped in a quoted string
      parseIdemTest addr_spec "\"test\r\n blah\"@example.com" -- This is a valid quoted string with folding white space
      parseIdemTest addr_spec "_Yosemite.Sam@example.com"
      parseIdemTest addr_spec "_somename@example.com"
      parseTest addr_spec "a(a(b(c)d(e(f))g)h(i)j)@example.com" `shouldReturn` "a@example.com"
      parseIdemTest addr_spec "a-b@bar.com"
      parseIdemTest addr_spec "a@b.co-foo.uk"
      parseIdemTest addr_spec "a@bar.com"
      parseIdemTest addr_spec "aaa@[123.123.123.123]"
      parseTest addr_spec "c@(Chris's host.)public.example" `shouldReturn` "c@public.example"
      parseTest addr_spec "cal(foo\\)bar)@iamcal.com"       `shouldReturn` "cal@iamcal.com"
      parseTest addr_spec "cal(foo\\@bar)@iamcal.com"       `shouldReturn` "cal@iamcal.com"
      parseTest addr_spec "cal(woo(yay)hoopla)@iamcal.com"  `shouldReturn` "cal@iamcal.com"
      parseTest addr_spec "cal@iamcal(woo).(yay)com"        `shouldReturn` "cal@iamcal.com"
      parseIdemTest addr_spec "customer/department=shipping@example.com"
      parseIdemTest addr_spec "customer/department@example.com"
      parseIdemTest addr_spec "dclo@us.ibm.com"
      parseTest addr_spec "first().last@example.com" `shouldReturn` "first.last@example.com"
      parseTest addr_spec "first(Welcome to\r\n the (\"wonderful\" (!)) world\r\n of email)@example.com" `shouldReturn` "first@example.com"
      parseTest addr_spec "first(a\"bc.def).last@example.com" `shouldReturn` "first.last@example.com"
      parseTest addr_spec "first(abc.def).last@example.com" `shouldReturn` "first.last@example.com"
      parseTest addr_spec "first(abc\\(def)@example.com" `shouldReturn` "first@example.com"
      parseTest addr_spec "first.(\")middle.last(\")@example.com" `shouldReturn` "first.middle.last@example.com"
      parseTest addr_spec "first.(\r\n middle\r\n )last@example.com" `shouldReturn` "first.last@example.com"
      parseIdemTest addr_spec "first.\"last\"@example.com" -- obs-local-part form as described in RFC 2822
      parseIdemTest addr_spec "first.\"mid\\dle\".\"last\"@example.com" -- Backslash can escape anything but must escape something
      parseIdemTest addr_spec "first.last@123.example.com"
      parseIdemTest addr_spec "first.last@1xample.com"
      parseIdemTest addr_spec "first.last@[12.34.56.78]"
      parseIdemTest addr_spec "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.56.78]"
      parseIdemTest addr_spec "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]"
      parseIdemTest addr_spec "first.last@[IPv6:1111:2222:3333:4444:5555:6666::]"
      parseIdemTest addr_spec "first.last@[IPv6:1111:2222:3333::4444:12.34.56.78]"
      parseIdemTest addr_spec "first.last@[IPv6:1111:2222:3333::4444:5555:6666]"
      parseIdemTest addr_spec "first.last@[IPv6:::1111:2222:3333:4444:5555:6666]"
      parseIdemTest addr_spec "first.last@[IPv6:::12.34.56.78]"
      parseIdemTest addr_spec "first.last@example.com"
      parseTest addr_spec "first.last@x(1234567890123456789012345678901234567890123456789012345678901234567890).com" `shouldReturn` "first.last@x.com"
      parseIdemTest addr_spec "first.last@x23456789012345678901234567890123456789012345678901234567890123.example.com"
      parseTest addr_spec "jdoe@machine(comment).  example" `shouldReturn` "jdoe@machine.example"
      parseIdemTest addr_spec "name.lastname@domain.com"
      parseTest addr_spec "pete(his account)@silly.test(his host)" `shouldReturn` "pete@silly.test"
      parseIdemTest addr_spec "peter.piper@example.com"
      parseIdemTest addr_spec "shaitan@my-domain.thisisminekthx" -- Disagree with Paul Gregg here
      parseIdemTest addr_spec "t*est@example.com"
      parseIdemTest addr_spec "test+test@example.com"
      parseIdemTest addr_spec "test-test@example.com"
      parseTest addr_spec "test. \r\n \r\n obs@syntax.com" `shouldReturn` "test.obs@syntax.com"
      parseTest addr_spec "test.\"test\"@example.com" `shouldReturn` "test.\"test\"@example.com"
      parseTest addr_spec "test.\r\n \r\n obs@syntax.com" `shouldReturn` "test.obs@syntax.com"
      parseIdemTest addr_spec "test.test@example.com"
      parseIdemTest addr_spec "test@123.123.123.x123"
      parseIdemTest addr_spec "test@[123.123.123.123]"
      parseIdemTest addr_spec "test@example.com"
      parseIdemTest addr_spec "test@example.example.com"
      parseIdemTest addr_spec "test@example.example.example.com"
      parseIdemTest addr_spec "user%uucp!path@somehost.edu"
      parseIdemTest addr_spec "user+mailbox@example.com"
      parseIdemTest addr_spec "valid@special.museum"
      parseIdemTest addr_spec "x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x234"
      parseIdemTest addr_spec "{_test_}@example.com"
      parseIdemTest addr_spec "~@example.com"

  describe "Rfc2822.path" $ do
    it "parses hand-picked inputs correctly" $
      parseTest path "  <joe@example.de>  " `shouldReturn` "<joe@example.de>"
    it "loses the route-part of an obsolete routing address" $
      parseTest path "<@example1.org,@example2.org:joe@example.org>" `shouldReturn` "<joe@example.org>"

  describe "Rfc2822.dot_atom" $ do
    it "consumes leading and trailing whitespace" $
      parseTest dot_atom " first.last " `shouldReturn` "first.last"
    it "does not allow interspersed whitespace" $ do
      parseFailure dot_atom "first . last"
      parseFailure dot_atom "first .last"
      parseFailure dot_atom "first. last"

  describe "Rfc2822.local_part" $ do
    it "consumes leading and trailing whitespace" $
      parseTest local_part " first.last " `shouldReturn` "first.last"
    it "consumes interspersed whitespace (obsolete syntax)" $ do
      parseTest local_part " first . last " `shouldReturn` "first.last"
      parseTest local_part " first .last " `shouldReturn` "first.last"
      parseTest local_part " first. last " `shouldReturn` "first.last"

  describe "Rfc2822.return_path" $ do
    it "parses hand-picked inputs correctly" $ do
      parseTest return_path "Return-Path: <joe@example.de>\r\n" `shouldReturn` "<joe@example.de>"
      parseTest return_path "Return-Path: <>\r\n" `shouldReturn` "<>"
    it "loses the route-part of an obsolete routing address" $
      parseTest return_path "Return-Path: <@example1.org,@example2.org:joe@example.org>\r\n" `shouldReturn` "<joe@example.org>"

  describe "Rfc2822.word" $
    it "parses hand-picked inputs correctly" $
      parseTest word "  foobar  " `shouldReturn` "foobar"

  describe "Rfc2822.body" $
    it "parses 8-bit characters correctly" $
      parseIdemTest body "abc äöüß def"
