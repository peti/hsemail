module Main (main) where

import Text.ParserCombinators.Parsec ( parse )
import Text.ParserCombinators.Parsec.Rfc2821

-- Read an SMTP command from standard input, parse it,
-- return the result, and loop until EOF.

main :: IO ()
main = do
  input <- getContents
  mapM (print . parse smtpCmd "") [ l ++ "\r\n" | l <- lines input ]
  return ()
