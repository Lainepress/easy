{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Text.RE.Internal.SearchReplace.TDFA.Sequence
  ( ed
  , edMS
  , edMI
  , edBS
  , edBI
  , edMultilineSensitive
  , edMultilineInsensitive
  , edBlockSensitive
  , edBlockInsensitive
  , ed_
  ) where

import qualified Data.Sequence                 as S
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.RE.TDFA.RE
import           Text.RE.Internal.SearchReplace.TDFAEdPrime
import           Text.RE.SearchReplace
import           Text.RE.Types.REOptions


-- | the @[ed| ... /// ... |]@ quasi quoters
ed
  , edMS
  , edMI
  , edBS
  , edBI
  , edMultilineSensitive
  , edMultilineInsensitive
  , edBlockSensitive
  , edBlockInsensitive
  , ed_ :: QuasiQuoter

ed                       = ed' sr_cast $ Just minBound
edMS                     = edMultilineSensitive
edMI                     = edMultilineInsensitive
edBS                     = edBlockSensitive
edBI                     = edBlockInsensitive
edMultilineSensitive     = ed' sr_cast $ Just  MultilineSensitive
edMultilineInsensitive   = ed' sr_cast $ Just  MultilineInsensitive
edBlockSensitive         = ed' sr_cast $ Just  BlockSensitive
edBlockInsensitive       = ed' sr_cast $ Just  BlockInsensitive
ed_                      = ed' fn_cast   Nothing

sr_cast :: Q Exp
sr_cast = [|\x -> x :: SearchReplace RE (S.Seq Char)|]

fn_cast :: Q Exp
fn_cast = [|\x -> x :: SimpleREOptions -> SearchReplace RE (S.Seq Char)|]
