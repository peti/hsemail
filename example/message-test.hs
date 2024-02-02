module Main (main) where

import Text.Parsec ( parse )
import Text.Parsec.Rfc2822

-- Read an Internet message from standard input, parse it,
-- and return the result.

main :: IO ()
main = do
  input <- getContents
  print $ parse message "<stdin>" (fixEol input)
  return ()

-- Make sure all lines are terminated by CRLF.

fixEol :: String -> String
fixEol ('\r':'\n':xs)   = '\r' : '\n' : fixEol xs
fixEol ('\n':xs)        = '\r' : '\n' : fixEol xs
fixEol (x:xs)           = x : fixEol xs
fixEol []               = []
