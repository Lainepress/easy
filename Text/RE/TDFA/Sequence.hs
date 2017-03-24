{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# OPTIONS_GHC -fno-warn-orphans           #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE CPP                            #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.TDFA.Sequence
  (
  -- * Tutorial
  -- $tutorial
  -- * The Match Operators
    (*=~)
  , (?=~)
  -- * The SearchReplace Operators
  , (*=~/)
  , (?=~/)
  -- * The Classic rexex-base Match Operators
  , (=~)
  , (=~~)
  -- * Matches
  , Matches
  , matchesSource
  , allMatches
  , anyMatches
  , countMatches
  , matches
  -- * Match
  , Match
  , matchSource
  , matched
  , matchedText
  -- * The 'RE' Type and functions
  -- $re
  , RE
  , SimpleREOptions(..)
  , reSource
  , compileRegex
  , compileRegexWith
  , escape
  , module Text.RE.TDFA.RE
  ) where

import           Prelude.Compat
import qualified Data.Sequence                 as S
import           Data.Typeable
import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.Types.IsRegex
import           Text.RE.Types.REOptions
import           Text.RE.Types.Replace
import           Text.RE.TDFA.RE
import           Text.RE.Types.SearchReplace
import qualified Text.Regex.TDFA               as TDFA


-- | find all matches in text
(*=~) :: (S.Seq Char)
      -> RE
      -> Matches (S.Seq Char)
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find first match in text
(?=~) :: (S.Seq Char)
      -> RE
      -> Match (S.Seq Char)
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | search and replace once
(?=~/) :: (S.Seq Char) -> SearchReplace RE (S.Seq Char) -> (S.Seq Char)
(?=~/) = flip searchReplaceFirst

-- | search and replace, all occurrences
(*=~/) :: (S.Seq Char) -> SearchReplace RE (S.Seq Char) -> (S.Seq Char)
(*=~/) = flip searchReplaceAll

-- | the regex-base polymorphic match operator
(=~) :: ( Typeable a
        , RegexContext TDFA.Regex (S.Seq Char) a
        , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
        )
     => (S.Seq Char)
     -> RE
     -> a
(=~) bs rex = addCaptureNames (reCaptureNames rex) $ match (reRegex rex) bs

-- | the regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , Functor m
         , Typeable a
         , RegexContext TDFA.Regex (S.Seq Char) a
         , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
         )
      => (S.Seq Char)
      -> RE
      -> m a
(=~~) bs rex = addCaptureNames (reCaptureNames rex) <$> matchM (reRegex rex) bs

instance IsRegex RE (S.Seq Char) where
  matchOnce     = flip (?=~)
  matchMany     = flip (*=~)
  makeRegexWith = \o -> compileRegexWith o . unpackR
  makeRegex     = compileRegex . unpackR
  regexSource   = packR . reSource

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>. These API
-- docs are mainly for reference.

-- $re
--
-- "Text.RE.TDFA.RE" contains the toolkit specific to the 'RE' type,
-- the type generated by the gegex compiler.
