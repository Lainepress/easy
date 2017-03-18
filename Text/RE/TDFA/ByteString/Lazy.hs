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

module Text.RE.TDFA.ByteString.Lazy
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
  -- * The Toolkit
  -- $toolkit
  , module Text.RE
  -- * The 'RE' Type and functions
  -- $re
  , RE
  , SimpleRegexOptions(..)
  , reSource
  , compileRegex
  , compileRegexWith
  , escape
  , module Text.RE.TDFA.RE
  ) where

import           Prelude.Compat
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Typeable
import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.Types.IsRegex
import           Text.RE.Types.Options
import           Text.RE.Types.Replace
import           Text.RE.TDFA.RE
import           Text.RE.Types.SearchReplace
import qualified Text.Regex.TDFA               as TDFA


-- | find all matches in text
(*=~) :: LBS.ByteString
      -> RE
      -> Matches LBS.ByteString
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find first match in text
(?=~) :: LBS.ByteString
      -> RE
      -> Match LBS.ByteString
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

(*=~/), (?=~/) :: LBS.ByteString -> SearchReplace RE LBS.ByteString -> LBS.ByteString
(?=~/) = flip searchReplaceFirst -- ^ search and replace once
(*=~/) = flip searchReplaceAll   -- ^ search and replace, all occurrences

-- | the regex-base polymorphic match operator
(=~) :: ( Typeable a
        , RegexContext TDFA.Regex LBS.ByteString a
        , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
        )
     => LBS.ByteString
     -> RE
     -> a
(=~) bs rex = addCaptureNames (reCaptureNames rex) $ match (reRegex rex) bs

-- | the regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , Functor m
         , Typeable a
         , RegexContext TDFA.Regex LBS.ByteString a
         , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
         )
      => LBS.ByteString
      -> RE
      -> m a
(=~~) bs rex = addCaptureNames (reCaptureNames rex) <$> matchM (reRegex rex) bs

instance IsRegex RE LBS.ByteString where
  matchOnce     = flip (?=~)
  matchMany     = flip (*=~)
  makeRegexWith = \o -> compileRegexWith o . unpackE
  makeRegex     = compileRegex . unpackE
  regexSource   = packE . reSource

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>. These API
-- docs are mainly for reference.

-- $toolkit
--
-- Beyond the above match operators and the regular expression type
-- below, "Text.RE" contains the toolkit for replacing captures,
-- specifying options, etc.

-- $re
--
-- "Text.RE.TDFA.RE" contains the toolkit specific to the 'RE' type,
-- the type generated by the gegex compiler.
