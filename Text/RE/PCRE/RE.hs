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

module Text.RE.PCRE.RE
  ( re
  , reMS
  , reMI
  , reBS
  , reBI
  , reMultilineSensitive
  , reMultilineInsensitive
  , reBlockSensitive
  , reBlockInsensitive
  , re_
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
  , cp
  , regexType
  , RE
  , reOptions
  , reSource
  , reCaptureNames
  , reRegex
  , Options
  , prelude
  , preludeEnv
  , preludeTestsFailing
  , preludeTable
  , preludeSummary
  , preludeSources
  , preludeSource
  , noPreludeOptions
  , defaultOptions
  , unpackSimpleRegexOptions
  , compileRegex
  , compileRegexWith
  , compileRegexWithOptions
  , escape
  , escapeREString
  ) where

import           Data.Bits
import           Data.Functor.Identity
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude.Compat
import           Text.RE.Internal.EscapeREString
import           Text.RE.Internal.NamedCaptures
import           Text.RE.Internal.PreludeMacros
import           Text.RE.Internal.QQ
import           Text.RE.TestBench
import           Text.RE.Types.CaptureID
import           Text.RE.Types.IsRegex
import           Text.RE.Types.Options
import           Text.RE.Types.Replace
import           Text.RE.Types.SearchReplace
import           Text.Regex.PCRE


re
  , reMS
  , reMI
  , reBS
  , reBI
  , reMultilineSensitive
  , reMultilineInsensitive
  , reBlockSensitive
  , reBlockInsensitive
  , re_
  , ed
  , edMS
  , edMI
  , edBS
  , edBI
  , edMultilineSensitive
  , edMultilineInsensitive
  , edBlockSensitive
  , edBlockInsensitive
  , ed_ :: QuasiQuoter

re                       = re' $ Just minBound
reMS                     = reMultilineSensitive
reMI                     = reMultilineInsensitive
reBS                     = reBlockSensitive
reBI                     = reBlockInsensitive
reMultilineSensitive     = re' $ Just  MultilineSensitive
reMultilineInsensitive   = re' $ Just  MultilineInsensitive
reBlockSensitive         = re' $ Just  BlockSensitive
reBlockInsensitive       = re' $ Just  BlockInsensitive
re_                      = re'   Nothing

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

regexType :: RegexType
regexType =
  mkPCRE $ \txt env md -> txt =~ mdRegexSource regexType ExclCaptures env md

data RE =
  RE
    { _re_options :: !Options
    , _re_source  :: !String
    , _re_cnames  :: !CaptureNames
    , _re_regex   :: !Regex
    }

reOptions :: RE -> Options
reOptions = _re_options

reSource :: RE -> String
reSource = _re_source

reCaptureNames :: RE -> CaptureNames
reCaptureNames = _re_cnames

reRegex  :: RE -> Regex
reRegex = _re_regex

type Options = Options_ RE CompOption ExecOption

instance IsOption SimpleRegexOptions RE CompOption ExecOption where
  makeOptions    = unpackSimpleRegexOptions

instance IsOption (Macros RE) RE CompOption ExecOption where
  makeOptions ms = Options ms def_comp_option def_exec_option

instance IsOption CompOption  RE CompOption ExecOption where
  makeOptions co = Options prelude co def_exec_option

instance IsOption ExecOption  RE CompOption ExecOption where
  makeOptions eo = Options prelude def_comp_option eo

instance IsOption Options     RE CompOption ExecOption where
  makeOptions    = id

instance IsOption ()          RE CompOption ExecOption where
  makeOptions _  = unpackSimpleRegexOptions minBound

def_comp_option :: CompOption
def_comp_option = optionsComp defaultOptions

def_exec_option :: ExecOption
def_exec_option = optionsExec defaultOptions

noPreludeOptions :: Options
noPreludeOptions = defaultOptions { optionsMacs = emptyMacros }

defaultOptions :: Options
defaultOptions = makeOptions (minBound::SimpleRegexOptions)

unpackSimpleRegexOptions :: SimpleRegexOptions -> Options
unpackSimpleRegexOptions sro =
  Options
    { optionsMacs = prelude
    , optionsComp = comp
    , optionsExec = defaultExecOpt
    }
  where
    comp =
      wiggle ml compMultiline $
      wiggle ci compCaseless
        defaultCompOpt

    wiggle True  m v = v .|.            m
    wiggle False m v = v .&. complement m

    (ml,ci) = case sro of
        MultilineSensitive    -> (,) True  False
        MultilineInsensitive  -> (,) True  True
        BlockSensitive        -> (,) False False
        BlockInsensitive      -> (,) False True

-- | compie a RE from a string using default options
compileRegex :: (Functor m,Monad m) => String -> m RE
compileRegex = compileRegexWithOptions ()

-- | compie a RE from a @String@ and 'SimpleRegexOptions'
compileRegexWith :: (Functor m,Monad m) => SimpleRegexOptions -> String -> m RE
compileRegexWith = compileRegexWithOptions

-- | compile a RE from a @String@ and a complete set of 'Options'
compileRegexWithOptions :: ( IsOption o RE CompOption ExecOption
                           , Functor m
                           , Monad   m
                           )
                        => o
                        -> String
                        -> m RE
compileRegexWithOptions = compileRegex_ . makeOptions

compileRegex_ :: ( Functor m , Monad m )
              => Options
              -> String
              -> m RE
compileRegex_ os re_s = uncurry mk <$> compileRegex' os re_s
  where
    mk cnms rex =
      RE
        { _re_options = os
        , _re_source  = re_s
        , _re_cnames  = cnms
        , _re_regex   = rex
        }

re' :: Maybe SimpleRegexOptions -> QuasiQuoter
re' mb = case mb of
  Nothing  ->
    (qq0 "re'")
      { quoteExp = parse minBound (\rs->[|flip unsafeCompileRegex rs|])
      }
  Just sro ->
    (qq0 "re'")
      { quoteExp = parse sro (\rs->[|unsafeCompileRegexSimple sro rs|])
      }
  where
    parse :: SimpleRegexOptions -> (String->Q Exp) -> String -> Q Exp
    parse sro mk rs = either error (\_->mk rs) $ compileRegex_ os rs
      where
        os = unpackSimpleRegexOptions sro

ed' :: Maybe SimpleRegexOptions -> QuasiQuoter
ed' mb = case mb of
  Nothing  ->
    (qq0 "ed'")
      { quoteExp = parse minBound (\rs->[|flip unsafeCompileSearchReplace rs|])
      }
  Just sro ->
    (qq0 "ed'")
      { quoteExp = parse sro (\rs->[|unsafeCompileSearchReplaceSimple sro rs|])
      }
  where
    parse :: SimpleRegexOptions -> (String->Q Exp) -> String -> Q Exp
    parse sro mk ts = either error (\_->mk ts) ei
      where
        ei :: Either String (SearchReplace RE String)
        ei = compileSearchReplace_ id (compileRegexWith sro) ts

unsafeCompileRegexSimple :: SimpleRegexOptions -> String -> RE
unsafeCompileRegexSimple sro re_s = unsafeCompileRegex os re_s
  where
    os = unpackSimpleRegexOptions sro

unsafeCompileRegex :: IsOption o RE CompOption ExecOption
                   => o
                   -> String
                   -> RE
unsafeCompileRegex = unsafeCompileRegex_ . makeOptions

unsafeCompileSearchReplaceSimple :: IsRegex RE s
                                 => SimpleRegexOptions
                                 -> String
                                 -> SearchReplace RE s
unsafeCompileSearchReplaceSimple sro =
    unsafeCompileSearchReplace $ unpackSimpleRegexOptions sro

unsafeCompileSearchReplace :: ( IsOption o RE CompOption ExecOption
                              , IsRegex RE s
                              )
                           => o
                           -> String
                           -> SearchReplace RE s
unsafeCompileSearchReplace os =
    unsafeCompileSearchReplace_ packE $ compileRegexWithOptions os

unsafeCompileRegex_ :: Options -> String -> RE
unsafeCompileRegex_ os = either oops id . compileRegexWithOptions os
  where
    oops = error . ("unsafeCompileRegex: " ++)

compileRegex' :: (Functor m,Monad m)
              => Options
              -> String
              -> m (CaptureNames,Regex)
compileRegex' Options{..} s0 = do
    ((_,cnms),s2) <- either fail return $ extractNamedCaptures s1
    (,) cnms <$> makeRegexOptsM optionsComp optionsExec s2
  where
    s1 = expandMacros reSource optionsMacs s0

prelude :: Macros RE
prelude = runIdentity $ preludeMacros mk regexType ExclCaptures
  where
    mk = Identity . unsafeCompileRegex_ noPreludeOptions

preludeTestsFailing :: [MacroID]
preludeTestsFailing = badMacros preludeEnv

preludeEnv :: MacroEnv
preludeEnv = preludeMacroEnv regexType

preludeTable :: String
preludeTable = preludeMacroTable regexType

preludeSummary :: PreludeMacro -> String
preludeSummary = preludeMacroSummary regexType

preludeSources :: String
preludeSources = preludeMacroSources regexType

preludeSource :: PreludeMacro -> String
preludeSource = preludeMacroSource regexType

escape :: (String->String) -> String -> RE
escape f = unsafeCompileRegex () . f . escapeREString
