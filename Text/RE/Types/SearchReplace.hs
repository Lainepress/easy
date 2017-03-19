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
  , unsafeCompileSearchReplace_
  , compileSearchReplace_
  ) where

import           Prelude.Compat
-- import           Language.Haskell.TH
-- import           Language.Haskell.TH.Quote
-- import           Text.RE.Internal.QQ
import           Text.RE.Types.IsRegex
-- import           Text.RE.Types.Options
import           Text.RE.Types.Replace


data SearchReplace re s =
  SearchReplace
    { getSearch   :: !re
    , getTemplate :: !s
    }

searchReplaceAll, searchReplaceFirst :: IsRegex re s => SearchReplace re s -> s -> s
searchReplaceAll   SearchReplace{..} = replaceAll getTemplate . matchMany getSearch
searchReplaceFirst SearchReplace{..} = replace    getTemplate . matchOnce getSearch

unsafeCompileSearchReplace_ :: (String->s)
                            -> (String->Either String re)
                            -> String
                            -> SearchReplace re s
unsafeCompileSearchReplace_ pk cf = either err id . compileSearchReplace_ pk cf
  where
    err msg = error $ "unsafeCompileSearchReplace_: " ++ msg

compileSearchReplace_ :: (String->s)
                      -> (String->Either String re)
                      -> String
                      -> m (SearchReplace re s)
compileSearchReplace_ = undefined


{-




searchReplaceQQParser :: (SimpleRegexOptions->String->Either String re)
                      -> (String -> s)
                      -> Maybe SimpleRegexOptions
                      -> QuasiQuoter
searchReplaceQQParser compile_re pack mb = case mb of
  Nothing  -> undefined
    -- (qq0 "searchReplaceQQParser")
    --   { quoteExp = parse minBound (\rs->[|flip (unsafe_compile_search_replace compile_re pack) rs|])
    --   }
  Just sro ->
    (qq0 "searchReplaceQQParser")
      { quoteExp = parse sro (\ts->[| foo undefined undefined undefined ts |])
      }
  where
    parse :: SimpleRegexOptions -> (String->Q Exp) -> String -> Q Exp
    parse sro mk ts = either error (\_->mk ts) $ compile_search_replace compile_re pack sro ts

    foo :: (SimpleRegexOptions->String->Bool) -> (String->s) -> SimpleRegexOptions -> String -> SearchReplace re s
    foo compile_re_ pack_ sro ts = undefined

    check_re :: SimpleRegexOptions -> String -> Bool
    check_re = undefined


      -- unsafe_compile_search_replace undefined pack_ sro ts

unsafe_compile_search_replace :: (SimpleRegexOptions->String->Maybe String)
                              -> (String->s)
                              -> SimpleRegexOptions
                              -> String
                              -> SearchReplace re s
unsafe_compile_search_replace compile_regex pack sro = undefined
  --   either err id . compile_search_replace compile_regex pack sro
  -- where
  --   err msg = error $ "unsafe_compile_search_replace: " ++ msg

compile_search_replace :: (SimpleRegexOptions->String->Either String re)
                       -> (String->s)
                       -> SimpleRegexOptions
                       -> String
                       -> Either String (SearchReplace re s)
compile_search_replace = undefined
-}
