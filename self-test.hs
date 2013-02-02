{-
   Module      :  Main
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   HsEmail regression test suite.
-}

module Main ( main ) where

import Test.Hspec
import System.Time ( CalendarTime(..), Month(..), Day(..) )
import Text.ParserCombinators.Parsec ( parse, eof, CharParser )
import Text.ParserCombinators.Parsec.Rfc2822

parseTest :: CharParser () a -> String -> IO a
parseTest p input = case parse (do { r <- p; eof; return r }) (show input) input of
                      Left err -> fail ("parse error at " ++ show err)
                      Right r -> return r

parseIdemTest :: CharParser () String -> String -> Expectation
parseIdemTest p input = parseTest p input `shouldReturn` input

parseFailure :: (Show a) => CharParser () a -> String -> Expectation
parseFailure p input = parse (do { r <- p; eof; return r }) (show input) input `shouldSatisfy` failure
  where
    failure (Left _) = True
    failure _        = False

main :: IO ()
main = hspec $ do
  describe "Rfc2822.date_time" $
    it "parses hand-picked times correctly" $
      parseTest date_time "Fri, 21 Dec 2012 00:07:43 +0300" `shouldReturn`
        CalendarTime 2012 December 21 0 7 43 0 Friday 0 "" 10800 False

  describe "Rfc2822.day" $ do
    it "parses a hand-picked day-of-months correctly" $ do
      parseTest day "00" `shouldReturn` 0
      parseTest day "09" `shouldReturn` 9
      parseTest day "15" `shouldReturn` 15

    it "does not perform range checking" $
      parseTest day "99" `shouldReturn` 99

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

  describe "Rfc2822.addr_spec" $
    it "parses hand-picked inputs correctly" $
      parseTest addr_spec "joe@example.de" `shouldReturn` "joe@example.de"

  describe "Rfc2822.path" $ do
    it "parses hand-picked inputs correctly" $
      parseTest path "  <joe@example.de>  " `shouldReturn` "<joe@example.de>"
    it "loses the route-part of an obsolete routing address" $
      parseTest path "<@example1.org,@example2.org:joe@example.org>" `shouldReturn` "<joe@example.org>"

  describe "Rfc2822.return_path" $ do
    it "parses hand-picked inputs correctly" $ do
      parseTest return_path "Return-Path: <joe@example.de>\r\n" `shouldReturn` "<joe@example.de>"
      parseTest return_path "Return-Path: <>\r\n" `shouldReturn` "<>"
    it "loses the route-part of an obsolete routing address" $
      parseTest return_path "Return-Path: <@example1.org,@example2.org:joe@example.org>\r\n" `shouldReturn` "<joe@example.org>"

  describe "Rfc2822.word" $
    it "parses hand-picked inputs correctly" $
      parseTest word "  foobar  " `shouldReturn` "foobar"
