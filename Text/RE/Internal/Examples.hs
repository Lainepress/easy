module Text.RE.Internal.Examples
  (
  -- * regex-examples dummy library
  -- $announce
    greetings
  ) where

greetings :: IO ()
greetings = putStr $ unlines
    [ "Hello! This is a dummy function in the dummy library of the"
    , "regex-examples package."
    ]

-- $announce
--
-- This is a dummy library to accompany the regex-examples programs.
