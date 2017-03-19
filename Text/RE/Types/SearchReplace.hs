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

import qualified Data.HashMap.Strict            as HMS
import           Prelude.Compat
import           Text.RE.Internal.NamedCaptures
import           Text.RE.Types.Capture
import           Text.RE.Types.CaptureID
import           Text.RE.Types.IsRegex
import           Text.RE.Types.Matches
import           Text.RE.Types.Replace
import qualified Text.Regex.TDFA                as TDFA


data SearchReplace re s =
  SearchReplace
    { getSearch   :: !re
    , getTemplate :: !s
    }

-- | search and replace
searchReplaceAll, searchReplaceFirst :: IsRegex re s => SearchReplace re s -> s -> s
searchReplaceAll   SearchReplace{..} = replaceAll getTemplate . matchMany getSearch
searchReplaceFirst SearchReplace{..} = replace    getTemplate . matchOnce getSearch

-- | warapper on 'compileSearchReplace_' that will generate an error
-- if any compilation errors are found
unsafeCompileSearchReplace_ :: (String->s)
                            -> (String->Either String re)
                            -> String
                            -> SearchReplace re s
unsafeCompileSearchReplace_ pk cf = either err id . compileSearchReplace_ pk cf
  where
    err msg = error $ "unsafeCompileSearchReplace_: " ++ msg

-- | compile a SearchReplace template generating errors if the RE or
-- the tempate are not well formed -- all capture references being checked
compileSearchReplace_ :: (Monad m,Functor m)
                      => (String->s)
                      -> (String->Either String re)
                      -> String
                      -> m (SearchReplace re s)
compileSearchReplace_ pack compile_re sr_tpl = either fail return $ do
    case mainCaptures $ sr_tpl $=~ "\\]/\\[" of
      [cap] -> compile_sr pack compile_re (capturePrefix cap) (captureSuffix cap)
      _      -> Left $ "bad search-replace template syntax: " ++ sr_tpl

compile_sr :: (String->s)
           -> (String->Either String re)
           -> String
           -> String
           -> Either String (SearchReplace re s)
compile_sr pack compile_re re_s tpl = do
    re           <- compile_re re_s
    ((n,cnms),_) <- extractNamedCaptures re_s
    mapM_ (check n cnms) $ templateCaptures id tpl
    return $ SearchReplace re $ pack tpl
  where
    check :: Int -> CaptureNames -> CaptureID -> Either String ()
    check n cnms cid = case cid of
      IsCaptureOrdinal co -> check_co n    co
      IsCaptureName    cn -> check_cn cnms cn

    check_co n (CaptureOrdinal i) = case i <= n of
      True  -> return ()
      False -> Left $ "capture ordinal out of range: " ++
                                      show i ++ " >= " ++ show n

    check_cn cnms cnm = case cnm `HMS.member` cnms of
      True  -> return ()
      False -> Left $ "capture name not defined: " ++
                                      show (getCaptureName cnm)

($=~) :: String -> String -> Matches String
($=~) = (TDFA.=~)
