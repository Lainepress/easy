{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.RE.Types.SearchReplace
  (
  -- * Serach and Replace
    SearchReplace(..)
  , searchReplaceFirst
  , searchReplaceAll
  , searchReplaceFirstMany
  , searchReplaceAllMany
  -- * The QuasiQuoters
  , ed
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

import           Prelude.Compat
--import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.RE.Types.IsRegex
import           Text.RE.Types.Options
import           Text.RE.Types.Replace


data SearchReplace re s =
  SearchReplace
    { getSearch   :: !re
    , getTemplate :: !s
    }

searchReplaceAll, searchReplaceFirst :: IsRegex re s => SearchReplace re s -> s -> s
searchReplaceAll   SearchReplace{..} = replaceAll getTemplate . matchMany getSearch
searchReplaceFirst SearchReplace{..} = replace    getTemplate . matchOnce getSearch

searchReplaceFirstMany, searchReplaceAllMany :: IsRegex re s => [SearchReplace re s] -> s -> s
searchReplaceFirstMany = compose . map searchReplaceFirst
searchReplaceAllMany   = compose . map searchReplaceAll

compose :: [a->a] -> a -> a
compose = foldr (.) id


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

ed                       = ed' $ Just minBound
edMS                     = edMultilineSensitive
edMI                     = edMultilineInsensitive
edBS                     = edBlockSensitive
edBI                     = edBlockInsensitive
edMultilineSensitive     = ed' $ Just  MultilineSensitive
edMultilineInsensitive   = ed' $ Just  MultilineInsensitive
edBlockSensitive         = ed' $ Just  BlockSensitive
edBlockInsensitive       = ed' $ Just  BlockInsensitive
ed_                      = ed'   Nothing

ed' :: Maybe SimpleRegexOptions -> QuasiQuoter
ed' = undefined
