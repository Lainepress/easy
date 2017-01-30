{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.RE.TDFA.Text
  ( (*=~)
  , (?=~)
  , (=~)
  , (=~~)
  , module Text.RE
  , module Text.RE.TDFA.RE
  ) where

import qualified Data.Text                     as T
import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.TDFA.RE
import qualified Text.Regex.TDFA               as TDFA


-- | find all matches in text
(*=~) :: T.Text
      -> RE
      -> Matches T.Text
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find first matches in text
(?=~) :: T.Text
      -> RE
      -> Match T.Text
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | regex-base polymorphic match operator
(=~) :: ( RegexContext TDFA.Regex T.Text a
        , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
        )
     => T.Text
     -> RE
     -> a
(=~) bs rex = match (reRegex rex) bs

-- | regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , RegexContext TDFA.Regex T.Text a
         , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
         )
      => T.Text
      -> RE
      -> m a
(=~~) bs rex = matchM (reRegex rex) bs

instance IsRegex RE T.Text where
  matchOnce   = flip (?=~)
  matchMany   = flip (*=~)
  regexSource = reSource
