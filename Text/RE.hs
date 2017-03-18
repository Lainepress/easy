{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
-- |
-- Module      :  Text.RE
-- Copyright   :  (C) 2016-17 Chris Dornan
-- License     :  BSD3 (see the LICENSE file)
-- Maintainer  :  Chris Dornan <chris.dornan@irisconnect.com>
-- Stability   :  RFC
-- Portability :  portable

module Text.RE
  (
  -- * The Tutorial
  -- $tutorial

  -- * How to use this library
  -- $use

  -- ** The Match Operators
  -- $operators

  -- * Matches
    Matches
  , matchesSource
  , allMatches
  , anyMatches
  , countMatches
  , matches
  -- ** Match
  , Match
  , matchSource
  , matched
  , matchedText
  ) where

import           Text.RE.Types.Match
import           Text.RE.Types.Matches

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>.

-- $use
--
-- This module won't provide any operators to match a regular expression
-- against text as it merely provides the toolkit for working with the
-- output of the match operators.  You probably won't import it directly
-- but import one of the modules that provides the match operators,
-- which will in tuen re-export this module.
--
-- The module that you choose to import will depend upon two factors:
--
-- * Which flavour of regular expression do you want to use? If you want
--   Posix flavour REs then you want the TDFA modules, otherwise its
--   PCRE for Perl-style REs.
--
-- * What type of text do you want to match: (slow) @String@s, @ByteString@,
--   @ByteString.Lazy@, @Text@, @Text.Lazy@ or the anachronistic @Seq Char@
--   or indeed a good old-fashioned polymorphic operators?
--
-- While we aim to provide all combinations of these choices, some of them
-- are currently not available.  In the regex package we have:
--
-- * "Text.RE.TDFA"
-- * "Text.RE.TDFA.ByteString"
-- * "Text.RE.TDFA.ByteString.Lazy"
-- * "Text.RE.TDFA.RE"
-- * "Text.RE.TDFA.Sequence"
-- * "Text.RE.TDFA.String"
-- * "Text.RE.TDFA.Text"
-- * "Text.RE.TDFA.Text.Lazy"
--
-- The PCRE modules are contained in the separate @regex-with-pcre@
-- package:
--
-- * "Text.RE.PCRE"
-- * "Text.RE.PCRE.ByteString"
-- * "Text.RE.PCRE.ByteString.Lazy"
-- * "Text.RE.PCRE.RE"
-- * "Text.RE.PCRE.Sequence"
-- * "Text.RE.PCRE.String"

-- $operators
--
-- The traditional @=~@ and @=~~@ operators are exported by the @regex@,
-- but we recommend that you use the two new operators, especially if
-- you are not familiar with the old operators.  We have:
--
-- * @txt ?=~ re@ searches for a single match yielding a value of type
--   'Match' @a@ where @a@ is the type of the text you are searching.
--
-- * @txt *=~ re@ searches for all non-overlapping matches in @txt@,
--   returning a value of type 'Matches' @a@.
--
-- See the sections below for more information on these @Matches@ and
-- @Match@ result types.
