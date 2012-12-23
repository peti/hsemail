module Main ( main ) where

import Test.Hspec
import System.Time ( CalendarTime(..), Month(..), Day(..) )
import Text.ParserCombinators.Parsec ( parse, CharParser )
import Text.ParserCombinators.Parsec.Rfc2822

parseTest :: CharParser () a -> String -> IO a
parseTest p input = case parse p "<buffer>" input of
                      Left err -> fail ("parse error at " ++ show err)
                      Right r -> return r

parseFailure :: (Show a) => CharParser () a -> String -> Expectation
parseFailure p input = parse p "<buffer>" input `shouldSatisfy` failure
  where
    failure (Left _) = True
    failure _        = False

main :: IO ()
main = hspec $ do
  describe "Rfc822.date_time" $
    it "parses hand-picked times correctly" $
      parseTest date_time "Fri, 21 Dec 2012 00:07:43 +0300" `shouldReturn`
        CalendarTime 2012 December 21 0 7 43 0 Friday 0 "" 10800 False

  describe "Rfc822.day" $
    it "parses a hand-picked day-of-months correctly" $ do
      parseTest day "00" `shouldReturn` 0
      parseTest day "09" `shouldReturn` 9
      parseTest day "15" `shouldReturn` 15

  describe "Rfc822.day" $
    it "does not perform range checking" $
      parseTest day "99" `shouldReturn` 99

  describe "Rfc822.day" $
    it "fails properly on incomplete input" $ do
      parseFailure day "Mon"
      parseFailure day "Thu"

  describe "Rfc822.obs_mbox_list" $
    it "parses hand-picked inputs correctly" $ do
      parseTest obs_mbox_list "," `shouldReturn` []
      parseTest obs_mbox_list "Joe Doe <joe@example.org>,( \r\n bla),,jane@\r\n example.net \r\n (Jane Doe)," `shouldReturn`
        [NameAddr (Just "Joe Doe") "joe@example.org",NameAddr Nothing "jane@example.net"]

  describe "Rfc822.obs_mbox_list" $
    it "fails properly on incomplete input" $
      parseFailure obs_mbox_list "foo@example.org"
