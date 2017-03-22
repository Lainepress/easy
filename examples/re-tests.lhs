Regex Test Suite
================

All of the regex exampes are self-testing and together make up the
regex test suite run during development and over each release of the
test suite. But here we have the unit an small-check tests used to
systematically probe the library for weak points and guard against
regressions.


\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Main (main) where

import           Data.Array
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as LBS
import qualified Data.Foldable                  as F
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence                  as S
import           Data.String
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import           Data.Typeable
import           Language.Haskell.TH.Quote
import           Prelude.Compat
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck          as SC
import           Text.Heredoc
import qualified Text.Regex.PCRE                as PCRE_
import qualified Text.Regex.TDFA                as TDFA_
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.Internal.NamedCaptures
import           Text.RE.Internal.PreludeMacros
import qualified Text.RE.PCRE                   as PCRE
import           Text.RE.TDFA                   as TDFA
import           Text.RE.TestBench
import           Text.RE.Tools.Sed
import           Text.RE.Types.Capture
import           Text.RE.Types.CaptureID
import           Text.RE.Types.Match
import           Text.RE.Types.Matches
import           Text.RE.Types.Options
import           Text.RE.Types.Replace
import           Text.RE.Types.SearchReplace

import qualified Text.RE.PCRE.String            as P_ST
import qualified Text.RE.PCRE.ByteString        as P_BS
import qualified Text.RE.PCRE.ByteString.Lazy   as PLBS
import qualified Text.RE.PCRE.Sequence          as P_SQ

import qualified Text.RE.TDFA.String            as T_ST
import qualified Text.RE.TDFA.ByteString        as T_BS
import qualified Text.RE.TDFA.ByteString.Lazy   as TLBS
import qualified Text.RE.TDFA.Sequence          as T_SQ
import qualified Text.RE.TDFA.Text              as T_TX
import qualified Text.RE.TDFA.Text.Lazy         as TLTX
\end{code}


\begin{code}
main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ prelude_tests
    , parsing_tests
    , core_tests
    , replaceMethodstests
    , searchReplaceReplaceTests
    , options_tests
    , namedCapturesTestTree
    , many_tests
    , escapeTests
    , add_capture_names_tests
    , misc_tests
    ]
\end{code}


\begin{code}
-- | check that our self-testing macro environments are good
prelude_tests :: TestTree
prelude_tests = testGroup "Prelude"
  [ tc TDFA.regexType TDFA.preludeEnv
  , tc PCRE.regexType PCRE.preludeEnv
  ]
  where
    tc rty m_env =
      testCase (show rty) $ do
        dumpMacroTable "macros" rty m_env
        assertBool "testMacroEnv" =<< testMacroEnv "prelude" rty m_env
\end{code}


Core Match/Replace Tests
------------------------

<h3>test vectors</h3>

The core tests rely on these simple test vectors.

\begin{code}
-- | our standard test strings
str_, str' :: String
str_      = "a bbbb aa b"
str'      = "foo"

-- | standard test REs
regex_, regex_alt :: RE
regex_    = [re|(a+) (b+)|]
regex_alt = [re|(a+)|(b+)|]

-- | golden matches result 1
regex_str_matches :: Matches String
regex_str_matches =
  Matches
    { matchesSource = str_
    , allMatches =
        [ regex_str_match
        , regex_str_match_2
        ]
    }

-- | golden match result 1
regex_str_match :: Match String
regex_str_match =
  Match
    { matchSource   = str_
    , captureNames  = noCaptureNames
    , matchArray    = array (0,2)
        [ (0,Capture {captureSource = str_, capturedText = "a bbbb", captureOffset = 0, captureLength = 6})
        , (1,Capture {captureSource = str_, capturedText = "a"     , captureOffset = 0, captureLength = 1})
        , (2,Capture {captureSource = str_, capturedText = "bbbb"  , captureOffset = 2, captureLength = 4})
        ]
    }

-- | golden match result 2
regex_str_match_2 :: Match String
regex_str_match_2 =
  Match
    { matchSource   = str_
    , captureNames  = noCaptureNames
    , matchArray    = array (0,2)
        [ (0,Capture {captureSource = str_, capturedText = "aa b", captureOffset = 7 , captureLength = 4})
        , (1,Capture {captureSource = str_, capturedText = "aa"  , captureOffset = 7 , captureLength = 2})
        , (2,Capture {captureSource = str_, capturedText = "b"   , captureOffset = 10, captureLength = 1})
        ]
    }

-- | golden match result 2
regex_alt_str_matches :: Matches String
regex_alt_str_matches =
  Matches
    { matchesSource = str_
    , allMatches    =
        [ Match
            { matchSource   = str_
            , captureNames  = noCaptureNames
            , matchArray    = array (0,2)
                [ (0,Capture {captureSource = str_, capturedText = "a", captureOffset = 0, captureLength = 1})
                , (1,Capture {captureSource = str_, capturedText = "a", captureOffset = 0, captureLength = 1})
                , (2,Capture {captureSource = str_, capturedText = "", captureOffset = -1, captureLength = 0})
                ]
            }
        , Match
            { matchSource   = str_
            , captureNames  = noCaptureNames
            , matchArray    = array (0,2)
                [ (0,Capture {captureSource = str_, capturedText = "bbbb", captureOffset = 2 , captureLength = 4})
                , (1,Capture {captureSource = str_, capturedText = ""    , captureOffset = -1, captureLength = 0})
                , (2,Capture {captureSource = str_, capturedText = "bbbb", captureOffset = 2 , captureLength = 4})
                ]
            }
        , Match
            { matchSource   = str_
            , captureNames  = noCaptureNames
            , matchArray    = array (0,2)
                [ (0,Capture {captureSource = str_, capturedText = "aa", captureOffset = 7 , captureLength = 2})
                , (1,Capture {captureSource = str_, capturedText = "aa", captureOffset = 7 , captureLength = 2})
                , (2,Capture {captureSource = str_, capturedText = ""  , captureOffset = -1, captureLength = 0})
                ]
            }
        , Match
            { matchSource   = str_
            , captureNames  = noCaptureNames
            , matchArray    = array (0,2)
                [ (0,Capture {captureSource = str_, capturedText = "b", captureOffset = 10, captureLength = 1})
                , (1,Capture {captureSource = str_, capturedText = "" , captureOffset = -1, captureLength = 0})
                , (2,Capture {captureSource = str_, capturedText = "b", captureOffset = 10, captureLength = 1})
                ]
            }
        ]
    }
\end{code}

<h3>testing the compileRegex functions</h3>

\begin{code}
parsing_tests :: TestTree
parsing_tests = testGroup "Parsing"
  [ testCase "complete check (matchM/ByteString)" $ do
      r <- TDFA.compileRegex $ reSource regex_
      assertEqual "Match" (B.pack <$> regex_str_match) $ B.pack str_ ?=~ r
  , testCase "matched (matchM/Text)" $ do
      r <- TDFA.compileRegex $ reSource regex_
      assertBool "matched" $ matched $ T.pack str_ ?=~ r
  , tc "TDFA.String"  TDFA.compileRegex
  , tc "TDFA.B"     $ TDFA.compileRegex . B.unpack
  , tc "TDFA.LBS"   $ TDFA.compileRegex . LBS.unpack
  , tc "TDFA.T"     $ TDFA.compileRegex . T.unpack
  , tc "TDFA.LT"    $ TDFA.compileRegex . LT.unpack
  , tc "TDFA.S"     $ TDFA.compileRegex . s_toList
  , tc "PCRE.String"  PCRE.compileRegex
  , tc "PCRE.B"     $ PCRE.compileRegex . B.unpack
  , tc "PCRE.LBS"   $ PCRE.compileRegex . LBS.unpack
  , tc "PCRE.S"     $ PCRE.compileRegex . s_toList
  ]
  where
    tc :: IsRegex re s => String -> (s->IO re) -> TestTree
    tc lab mk0 = testGroup lab
        [ testCase "loop" $ do
            r <- mk re_s
            assertEqual "RE" re_s $ regexSource r
        , testCase "Match" $ do
            r <- mk' re_s
            assertEqual "Match" (pk <$> regex_str_match) $ matchOnce r $ pk str_
        ]
      where
        mk   = makeRegex              `asTypeOf` mk0

        mk'  = makeRegexWith minBound `asTypeOf` mk0

        re_s = pk $ reSource regex_

        pk   = mk_pk mk0

        mk_pk :: Replace s' => (s'->IO re') -> String -> s'
        mk_pk _ = packE
\end{code}

<h3>core tests</h3>

\begin{code}
core_tests :: TestTree
core_tests = testGroup "Match"
  [ testCase "text (=~~Text.Lazy)" $ do
      txt <- LT.pack str_ =~~ [re|(a+) (b+)|] :: IO (LT.Text)
      assertEqual "text" txt "a bbbb"
  , testCase "multi (=~~/String)" $ do
      let sm = str_ =~ regex_ :: Match String
          m  = capture [cp|0|] sm
      assertEqual "captureSource" "a bbbb aa b" $ captureSource m
      assertEqual "capturedText"  "a bbbb"      $ capturedText  m
      assertEqual "capturePrefix" ""            $ capturePrefix m
      assertEqual "captureSuffix" " aa b"       $ captureSuffix m
  , testCase "complete (=~~/ByteString)" $ do
      mtch <- B.pack str_ =~~ regex_ :: IO (Match B.ByteString)
      assertEqual "Match" mtch $ B.pack <$> regex_str_match
  , testCase "complete (all,String)" $ do
      let mtchs = str_ =~ regex_     :: Matches String
      assertEqual "Matches" mtchs regex_str_matches
  , testCase "complete (all,reg_alt)" $ do
      let mtchs = str_ =~ regex_alt  :: Matches String
      assertEqual "Matches" mtchs regex_alt_str_matches
  , testCase "complete (=~~,all)" $ do
      mtchs <- str_ =~~ regex_       :: IO (Matches String)
      assertEqual "Matches" mtchs regex_str_matches
  , testCase "fail (all)" $ do
      let mtchs = str' =~ regex_    :: Matches String
      assertEqual "not.anyMatches" False $ anyMatches mtchs
  ]
\end{code}

<h3>testing the replace functions at different types</h3>

\begin{code}
replaceMethodstests :: TestTree
replaceMethodstests = testGroup "Replace"
  [ testCase "String/single" $ do
      let m = str_ =~ regex_ :: Match String
          r = replaceCaptures ALL fmt m
      assertEqual "replaceCaptures" r "(0:0:(0:1:a) (0:2:bbbb)) aa b"
  , testCase "String/alt" $ do
      let ms = str_ =~ regex_ :: Matches String
          r  = replaceAllCaptures ALL fmt ms
      chk r
  , testCase "String" $ do
      let ms = str_ =~ regex_ :: Matches String
          r  = replaceAllCaptures ALL fmt ms
      chk r
  , testCase "ByteString" $ do
      let ms = B.pack str_ =~ regex_ :: Matches B.ByteString
          r  = replaceAllCaptures ALL fmt ms
      chk r
  , testCase "LBS.ByteString" $ do
      let ms = LBS.pack str_ =~ regex_ :: Matches LBS.ByteString
          r  = replaceAllCaptures ALL fmt ms
      chk r
  , testCase "Seq Char" $ do
      let ms = S.fromList str_ =~ regex_ :: Matches (S.Seq Char)
          f  = \_ (Location i j) Capture{..} -> Just $ S.fromList $
                  "(" <> show i <> ":" <> show_co j <> ":" <>
                    F.toList capturedText <> ")"
          r  = replaceAllCaptures ALL f ms
      assertEqual "replaceAllCaptures" r $
        S.fromList "(0:0:(0:1:a) (0:2:bbbb)) (1:0:(1:1:aa) (1:2:b))"
  , testCase "Text" $ do
      let ms = T.pack str_ =~ regex_ :: Matches T.Text
          r  = replaceAllCaptures ALL fmt ms
      chk r
  , testCase "LT.Text" $ do
      let ms = LT.pack str_ =~ regex_ :: Matches LT.Text
          r  = replaceAllCaptures ALL fmt ms
      chk r
  ]
  where
    chk r =
      assertEqual
        "replaceAllCaptures"
        r
        "(0:0:(0:1:a) (0:2:bbbb)) (1:0:(1:1:aa) (1:2:b))"

    fmt :: (IsString s,Replace s) => a -> Location -> Capture s -> Maybe s
    fmt _ (Location i j) Capture{..} = Just $ "(" <> packE (show i) <> ":" <>
      packE (show_co j) <> ":" <> capturedText <> ")"

    show_co (CaptureOrdinal j) = show j
\end{code}


<h3>SearchReplace</h3>

\begin{code}
searchReplaceReplaceTests :: TestTree
searchReplaceReplaceTests = testGroup "SearchReplace"
    [ testCase "TDFA.ed/String" $ test  id         tdfa_eds
    , testCase "PCRE.ed/String" $ test  id         pcre_eds
    , testCase "TDFA.ed/B"      $ test  B.pack     tdfa_eds
    , testCase "PCRE.ed/B"      $ test  B.pack     pcre_eds
    , testCase "TDFA.ed/LBS"    $ test  LBS.pack   tdfa_eds
    , testCase "PCRE.ed/LBS"    $ test  LBS.pack   pcre_eds
    , testCase "TDFA.ed/S"      $ test  S.fromList tdfa_eds
    , testCase "PCRE.ed/S"      $ test  S.fromList pcre_eds
    , testCase "TDFA.ed/T"      $ test  T.pack     tdfa_eds
    , testCase "TDFA.ed/LT"     $ test  LT.pack    tdfa_eds
    , testCase "TDFA.ed/T(d)"   $ test  T.pack     tdfa_eds'
    , testCase "PCRE.ed/LBS(d)" $ test  LBS.pack   pcre_eds'
    , testg "TDFA.op/String" (T_ST.?=~/) (T_ST.*=~/) tdfa_sr
    , testg "PCRE.op/String" (P_ST.?=~/) (P_ST.*=~/) pcre_sr
    , testg "TDFA.op/B"      (T_BS.?=~/) (T_BS.*=~/) tdfa_sr
    , testg "PCRE.op/B"      (P_BS.?=~/) (P_BS.*=~/) pcre_sr
    , testg "TDFA.op/LBS"    (TLBS.?=~/) (TLBS.*=~/) tdfa_sr
    , testg "PCRE.op/LBS"    (PLBS.?=~/) (PLBS.*=~/) pcre_sr
    , testg "TDFA.op/T"      (T_TX.?=~/) (T_TX.*=~/) tdfa_sr
    , testg "TDFA.op/LT"     (TLTX.?=~/) (TLTX.*=~/) tdfa_sr
    , testG "TDFA.op/S"      (T_SQ.?=~/) (T_SQ.*=~/) tdfa_sr
    , testG "PCRE.op/S"      (P_SQ.?=~/) (P_SQ.*=~/) pcre_sr
    ]
  where
    test :: IsRegex re a => (String->a) -> Edits Identity re a -> Assertion
    test inj eds =  inj rsm @=? runIdentity (sed' eds $ inj inp)

    testg lab op1 opm sr = testGroup lab
      [ testCase "?=~/" $ rs1 @=? inp `op1` sr
      , testCase "*=~/" $ rsm @=? inp `opm` sr
      ]

    testG lab op1 opm sr = testGroup lab
      [ testCase "?=~/" $ S.fromList rs1 @=? S.fromList inp `op1` sr
      , testCase "*=~/" $ S.fromList rsm @=? S.fromList inp `opm` sr
      ]

    inp, rs1, rsm :: IsString a => a
    inp = "16/03/2017 01/01/2000\n"
    rs1 = "2017-03-16 01/01/2000\n"
    rsm = "2017-03-16 2000-01-01\n"

    tdfa_eds :: IsRegex TDFA.RE a => Edits Identity TDFA.RE a
    tdfa_eds = Select [Template tdfa_sr]

    pcre_eds :: IsRegex PCRE.RE a => Edits Identity PCRE.RE a
    pcre_eds = Select [Template pcre_sr]

    tdfa_sr  :: IsRegex TDFA.RE a => SearchReplace TDFA.RE a
    tdfa_sr  = [TDFA.ed|${d}([0-9]{2})/${m}([0-9]{2})/${y}([0-9]{4})///${y}-${m}-${d}|]

    pcre_sr  :: IsRegex PCRE.RE a => SearchReplace PCRE.RE a
    pcre_sr  = [PCRE.ed|${d}([0-9]{2})/${m}([0-9]{2})/${y}([0-9]{4})///${y}-${m}-${d}|]

    tdfa_eds' :: IsRegex TDFA.RE a => Edits Identity TDFA.RE a
    tdfa_eds' = Select [Template $ TDFA.unsafeCompileSearchReplaceSimple minBound "${d}([0-9]{2})/${m}([0-9]{2})/${y}([0-9]{4})///${y}-${m}-${d}"]

    pcre_eds' :: IsRegex PCRE.RE a => Edits Identity PCRE.RE a
    pcre_eds' = Select [Template $ PCRE.unsafeCompileSearchReplaceSimple minBound "${d}([0-9]{2})/${m}([0-9]{2})/${y}([0-9]{4})///${y}-${m}-${d}"]

\end{code}


<h3>Testing The Options</h3>

\begin{code}
options_tests :: TestTree
options_tests = testGroup "Simple Options"
  [ testGroup "TDFA Simple Options"
      [ testCase "default (MultilineSensitive)" $ assertEqual "#" 2 $
          countMatches $ s TDFA.*=~ [TDFA.re|[0-9a-f]{2}$|]
      , testCase "MultilineSensitive" $ assertEqual "#" 2 $
          countMatches $ s TDFA.*=~ [TDFA.reMultilineSensitive|[0-9a-f]{2}$|]
      , testCase "MultilineInsensitive" $ assertEqual "#" 4 $
          countMatches $ s TDFA.*=~ [TDFA.reMultilineInsensitive|[0-9a-f]{2}$|]
      , testCase "BlockSensitive" $ assertEqual "#" 0 $
          countMatches $ s TDFA.*=~ [TDFA.reBlockSensitive|[0-9a-f]{2}$|]
      , testCase "BlockInsensitive" $ assertEqual "#" 1 $
          countMatches $ s TDFA.*=~ [TDFA.reBlockInsensitive|[0-9a-f]{2}$|]
      ]
  , testGroup "PCRE Simple Options"
      [ testCase "default (MultilineSensitive)" $ assertEqual "#" 2 $
          countMatches $ s PCRE.*=~ [PCRE.re|[0-9a-f]{2}$|]
      , testCase "MultilineSensitive" $ assertEqual "#" 2 $
          countMatches $ s PCRE.*=~ [PCRE.reMultilineSensitive|[0-9a-f]{2}$|]
      , testCase "MultilineInsensitive" $ assertEqual "#" 4 $
          countMatches $ s PCRE.*=~ [PCRE.reMultilineInsensitive|[0-9a-f]{2}$|]
      , testCase "BlockSensitive" $ assertEqual "#" 0 $
          countMatches $ s PCRE.*=~ [PCRE.reBlockSensitive|[0-9a-f]{2}$|]
      , testCase "BlockInsensitive" $ assertEqual "#" 1 $
          countMatches $ s PCRE.*=~ [PCRE.reBlockInsensitive|[0-9a-f]{2}$|]
      ]
    ]
  where
    s = "0a\nbb\nFe\nA5" :: String
\end{code}


<h3>Exercising Our Many APIs</h3>

\begin{code}
many_tests :: TestTree
many_tests = testGroup "Many Tests"
    [ testCase "PCRE a"               $ test (PCRE.*=~) (PCRE.?=~) (PCRE.=~) (PCRE.=~~) matchOnce matchMany id          re_pcre
    , testCase "PCRE ByteString"      $ test (P_BS.*=~) (P_BS.?=~) (P_BS.=~) (P_BS.=~~) matchOnce matchMany B.pack      re_pcre
    , testCase "PCRE ByteString.Lazy" $ test (PLBS.*=~) (PLBS.?=~) (PLBS.=~) (PLBS.=~~) matchOnce matchMany LBS.pack    re_pcre
    , testCase "PCRE Sequence"        $ test (P_SQ.*=~) (P_SQ.?=~) (P_SQ.=~) (P_SQ.=~~) matchOnce matchMany S.fromList  re_pcre
    , testCase "PCRE String"          $ test (P_ST.*=~) (P_ST.?=~) (P_ST.=~) (P_ST.=~~) matchOnce matchMany id          re_pcre
    , testCase "TDFA a"               $ test (TDFA.*=~) (TDFA.?=~) (TDFA.=~) (TDFA.=~~) matchOnce matchMany id          re_tdfa
    , testCase "TDFA ByteString"      $ test (T_BS.*=~) (T_BS.?=~) (T_BS.=~) (T_BS.=~~) matchOnce matchMany B.pack      re_tdfa
    , testCase "TDFA ByteString.Lazy" $ test (TLBS.*=~) (TLBS.?=~) (TLBS.=~) (TLBS.=~~) matchOnce matchMany LBS.pack    re_tdfa
    , testCase "TDFA Sequence"        $ test (T_SQ.*=~) (T_SQ.?=~) (T_SQ.=~) (T_SQ.=~~) matchOnce matchMany S.fromList  re_tdfa
    , testCase "TDFA String"          $ test (T_ST.*=~) (T_ST.?=~) (T_ST.=~) (T_ST.=~~) matchOnce matchMany id          re_tdfa
    , testCase "TDFA Text"            $ test (T_TX.*=~) (T_TX.?=~) (T_TX.=~) (T_TX.=~~) matchOnce matchMany T.pack      re_tdfa
    , testCase "TDFA Text.Lazy"       $ test (TLTX.*=~) (TLTX.?=~) (TLTX.=~) (TLTX.=~~) matchOnce matchMany LT.pack     re_tdfa
    ]
  where
    test :: (Show s,Eq s)
         => (s->r->Matches s)
         -> (s->r->Match   s)
         -> (s->r->Matches s)
         -> (s->r->Maybe(Match s))
         -> (r->s->Match   s)
         -> (r->s->Matches s)
         -> (String->s)
         -> r
         -> Assertion
    test (%*=~) (%?=~) (%=~) (%=~~) mo mm inj r = do
        2         @=? countMatches mtchs
        Just txt' @=? matchedText  mtch
        mtchs     @=? mtchs'
        mb_mtch   @=? Just mtch
        mtch      @=? mtch''
        mtchs     @=? mtchs''
      where
        mtchs   = txt %*=~ r
        mtch    = txt %?=~ r
        mtchs'  = txt %=~  r
        mb_mtch = txt %=~~ r
        mtch''  = mo r txt
        mtchs'' = mm r txt

        txt     = inj "2016-01-09 2015-12-5 2015-10-05"
        txt'    = inj "2016-01-09"

    re_pcre = fromMaybe oops $ PCRE.compileRegex "[0-9]{4}-[0-9]{2}-[0-9]{2}"
    re_tdfa = fromMaybe oops $ TDFA.compileRegex "[0-9]{4}-[0-9]{2}-[0-9]{2}"

    oops    = error "many_tests"
\end{code}


Testing the RE Escape Functions
-------------------------------

\begin{code}
escapeTests :: TestTree
escapeTests = testGroup "Escape Tests"
    [ testGroup "PCRE"
        [ testCase  "Escaping empty string" $
            assertBool "empty string" $
              tst P_ST.escape (P_ST.?=~) ""
        , testCase  "Escaping RE metacharacters" $
            assertBool "metacharacters" $
              tst P_ST.escape (P_ST.?=~) metacharacters
        , localOption (SmallCheckDepth 6) $
            SC.testProperty "matched $ <s> ?=~ [re|^escape(<s>)$|]" $
              tst P_ST.escape (P_ST.?=~)
        ]
    , testGroup "TDFA"
        [ testCase  "Escaping empty string" $
            assertBool "empty string" $
              tst T_ST.escape (T_ST.?=~) ""
        , testCase  "Escaping RE metacharacters" $
            assertBool "metacharacters" $
              tst T_ST.escape (T_ST.?=~) metacharacters
        , localOption (SmallCheckDepth 6) $
            SC.testProperty "matched $ <s> ?=~ [re|^escape(<s>)$|]" $
              tst T_ST.escape (T_ST.?=~)
        ]
    ]
  where
    tst :: ((String->String)->String->a)
        -> (String->a->Match String)
        -> String
        -> Bool
    tst esc (%=~) s = matched $ s %=~ esc (("^" ++) . (++ "$")) s

    metacharacters :: String
    metacharacters = "^\\.|*+?()[]{}$"
\end{code}


Named Capture Tests
-------------------

\begin{code}
namedCapturesTestTree :: TestTree
namedCapturesTestTree = localOption (SmallCheckDepth 4) $
  testGroup "NamedCaptures"
    [ formatScanTestTree
    , analyseTokensTestTree
    ]

instance Monad m => Serial m Token

formatScanTestTree :: TestTree
formatScanTestTree =
  testGroup "FormatToken/Scan Properties"
    [ localOption (SmallCheckDepth 4) $
        SC.testProperty "formatTokens == formatTokens0" $
          \tks -> formatTokens tks == formatTokens0 tks
    , localOption (SmallCheckDepth 4) $
        SC.testProperty "scan . formatTokens' idFormatTokenOptions == id" $
          \tks -> all validToken tks ==>
                    scan (formatTokens' idFormatTokenOptions tks) == tks
    ]

analyseTokensTestTree :: TestTree
analyseTokensTestTree =
  testGroup "Analysing [Token] Unit Tests"
    [ tc [here|foobar|]                                       []
    , tc [here||]                                             []
    , tc [here|$([0-9]{4})|]                                  []
    , tc [here|${x}()|]                                       [(1,"x")]
    , tc [here|${}()|]                                        []
    , tc [here|${}()${foo}()|]                                [(2,"foo")]
    , tc [here|${x}(${y()})|]                                 [(1,"x")]
    , tc [here|${x}(${y}())|]                                 [(1,"x"),(2,"y")]
    , tc [here|${a}(${b{}())|]                                [(1,"a")]
    , tc [here|${y}([0-9]{4})-${m}([0-9]{2})-${d}([0-9]{2})|] [(1,"y"),(2,"m"),(3,"d")]
    , tc [here|@$(@|\{${name}([^{}]+)\})|]                    [(2,"name")]
    , tc [here|${y}[0-9]{4}|]                                 []
    , tc [here|${}([0-9]{4})|]                                []
    ]
  where
    tc s al =
      testCase s $ assertEqual "CaptureNames"
        (xnc s)
        (HM.fromList
          [ (CaptureName $ T.pack n,CaptureOrdinal i)
              | (i,n)<-al
              ]
        )

    xnc = either oops (snd . fst) . extractNamedCaptures
      where
        oops = error "analyseTokensTestTree: unexpected parse failure"
\end{code}


AddCaptureNames Tests
---------------------

\begin{code}
add_capture_names_tests :: TestTree
add_capture_names_tests = testGroup "AddCaptureNames Tests"
    [ test_add_capture_name "Match   String"          test_match                    regex_str_match
    , test_add_capture_name "Matches String"          test_matches                  regex_str_matches
    , test_add_capture_name "Match   B.ByteString"    test_match   $ B.pack     <$> regex_str_match
    , test_add_capture_name "Matches B.ByteString"    test_matches $ B.pack     <$> regex_str_matches
    , test_add_capture_name "Match   LBS.ByteString"  test_match   $ LBS.pack   <$> regex_str_match
    , test_add_capture_name "Matches LBS.ByteString"  test_matches $ LBS.pack   <$> regex_str_matches
    , test_add_capture_name "Match   T.Text"          test_match   $ T.pack     <$> regex_str_match
    , test_add_capture_name "Matches T.Text"          test_matches $ T.pack     <$> regex_str_matches
    , test_add_capture_name "Match   LT.Text"         test_match   $ LT.pack    <$> regex_str_match
    , test_add_capture_name "Matches LT.Text"         test_matches $ LT.pack    <$> regex_str_matches
    , test_add_capture_name "Match   (Seq Char)"      test_match   $ S.fromList <$> regex_str_match
    , test_add_capture_name "Matches (Seq Char)"      test_matches $ S.fromList <$> regex_str_matches
    ]

test_matches :: CaptureNames -> Matches a -> Bool
test_matches cnms = all (test_match cnms) . allMatches

test_match :: CaptureNames -> Match a -> Bool
test_match cnms mtch = captureNames mtch == cnms

test_add_capture_name :: Typeable a
                      => String
                      -> (CaptureNames->a->Bool)
                      -> a
                      -> TestTree
test_add_capture_name lab tst x = testCase lab $
    assertBool lab $ tst cnms $ addCaptureNames cnms x
  where
    cnms = HM.fromList
      [ (CaptureName "x",1)
      , (CaptureName "y",2)
      ]
\end{code}


The Miscelaneous Tests
----------------------

\begin{code}
misc_tests :: TestTree
misc_tests = testGroup "Miscelaneous Tests"
    [ testGroup "QQ"
        [ qq_tc "re"                      re
        , qq_tc "reMS"                    reMS
        , qq_tc "reMI"                    reMI
        , qq_tc "reBS"                    reBS
        , qq_tc "reBI"                    reBI
        , qq_tc "reMultilineSensitive"    reMultilineSensitive
        , qq_tc "reMultilineInsensitive"  reMultilineInsensitive
        , qq_tc "reBlockSensitive"        reBlockSensitive
        , qq_tc "reBlockInsensitive"      reBlockInsensitive
        , qq_tc "re_"                     re_
        , qq_tc "ed"                      ed
        , qq_tc "edMS"                    edMS
        , qq_tc "edMI"                    edMI
        , qq_tc "edBS"                    edBS
        , qq_tc "edBI"                    edBI
        , qq_tc "edMultilineSensitive"    edMultilineSensitive
        , qq_tc "edMultilineInsensitive"  edMultilineInsensitive
        , qq_tc "edBlockSensitive"        edBlockSensitive
        , qq_tc "edBlockInsensitive"      edBlockInsensitive
        , qq_tc "ed_"                     ed_
        ]
    , testGroup "PreludeMacros"
        [ valid_string "preludeMacroTable"    preludeMacroTable
        , valid_macro  "preludeMacroSummary"  preludeMacroSummary
        , valid_string "preludeMacroSources"  preludeMacroSources
        , valid_macro  "preludeMacroSource"   preludeMacroSource
        ]
    -- because HPC can't measure our testing of [re|..|] forms,
    -- we are eliminating them from our enquiries
    , testGroup "RE"
        [ valid_res TDFA.regexType
            [ TDFA.re
            , TDFA.reMS
            , TDFA.reMI
            , TDFA.reBS
            , TDFA.reBI
            , TDFA.reMultilineSensitive
            , TDFA.reMultilineInsensitive
            , TDFA.reBlockSensitive
            , TDFA.reBlockInsensitive
            , TDFA.re_
            , TDFA.ed
            , TDFA.edMS
            , TDFA.edMI
            , TDFA.edBS
            , TDFA.edBI
            , TDFA.edMultilineSensitive
            , TDFA.edMultilineInsensitive
            , TDFA.edBlockSensitive
            , TDFA.edBlockInsensitive
            , TDFA.ed_
            ]
        , testCase  "TDFA.regexType"           $ assertBool "TDFA" $ isTDFA TDFA.regexType
        , testCase  "TDFA.reOptions"           $ assert_empty_macs $ optionsMacs (TDFA.reOptions tdfa_re)
        , testCase  "TDFA.makeOptions md"      $ assert_empty_macs $ optionsMacs tdfa_opts
        , testCase  "TDFA.preludeTestsFailing" $ []      @=? TDFA.preludeTestsFailing
        , ne_string "TDFA.preludeTable"          TDFA.preludeTable
        , ne_string "TDFA.preludeSources"        TDFA.preludeSources
        , testGroup "TDFA.preludeSummary"
            [ ne_string (presentPreludeMacro pm) $ TDFA.preludeSummary pm
                | pm <- tdfa_prelude_macros
                ]
        , testGroup  "TDFA.preludeSource"
            [ ne_string (presentPreludeMacro pm) $ TDFA.preludeSource  pm
                | pm <- tdfa_prelude_macros
                ]
    -- because HPC can't measure our testing of [re|..|] forms,
    -- we are eliminating them from our enquiries
        , valid_res PCRE.regexType
            [ PCRE.re
            , PCRE.reMS
            , PCRE.reMI
            , PCRE.reBS
            , PCRE.reBI
            , PCRE.reMultilineSensitive
            , PCRE.reMultilineInsensitive
            , PCRE.reBlockSensitive
            , PCRE.reBlockInsensitive
            , PCRE.re_
            , PCRE.ed
            , PCRE.edMS
            , PCRE.edMI
            , PCRE.edBS
            , PCRE.edBI
            , PCRE.edMultilineSensitive
            , PCRE.edMultilineInsensitive
            , PCRE.edBlockSensitive
            , PCRE.edBlockInsensitive
            , PCRE.ed_
            ]
        , testCase  "PCRE.regexType"           $ assertBool "PCRE" $ isPCRE PCRE.regexType
        , testCase  "PCRE.reOptions"           $ assert_empty_macs $ optionsMacs (PCRE.reOptions pcre_re)
        , testCase  "PCRE.makeOptions md"      $ assert_empty_macs $ optionsMacs pcre_opts
        , testCase  "PCRE.preludeTestsFailing" $ []      @=? PCRE.preludeTestsFailing
        , ne_string "PCRE.preludeTable"          PCRE.preludeTable
        , ne_string "PCRE.preludeTable"          PCRE.preludeSources
        , testGroup "PCRE.preludeSummary"
            [ ne_string (presentPreludeMacro pm) $ PCRE.preludeSummary pm
                | pm <- pcre_prelude_macros
                ]
        , testGroup "PCRE.preludeSource"
            [ ne_string (presentPreludeMacro pm) $ PCRE.preludeSource  pm
                | pm <- pcre_prelude_macros
                ]
        ]
    ]
  where
    tdfa_re   = fromMaybe oops $ TDFA.compileRegexWithOptions tdfa_opts ".*"
    pcre_re   = fromMaybe oops $ PCRE.compileRegexWithOptions pcre_opts ".*"

    tdfa_opts = makeOptions no_macs_t :: Options_ TDFA.RE TDFA_.CompOption TDFA_.ExecOption
    pcre_opts = makeOptions no_macs_p :: Options_ PCRE.RE PCRE_.CompOption PCRE_.ExecOption

    no_macs_t = HM.fromList [] :: Macros TDFA.RE
    no_macs_p = HM.fromList [] :: Macros PCRE.RE

    oops      = error "misc_tests"

    assert_empty_macs = assertBool "macros not empty" . HM.null

qq_tc :: String -> QuasiQuoter -> TestTree
qq_tc lab qq = testCase lab $ quoteExp qq `seq` assertBool "qq_tc" True

valid_macro :: String -> (RegexType->PreludeMacro->String) -> TestTree
valid_macro label f = testGroup label
    [ valid_string (presentPreludeMacro pm) (flip f pm)
        | pm<-[minBound..maxBound]
        ]

valid_string :: String -> (RegexType->String) -> TestTree
valid_string label f = testGroup label
    [ ne_string (presentRegexType rty) $ f rty
        | rty<-[TDFA.regexType] -- until PCRE has a binding for all macros
        ]

ne_string :: String -> String -> TestTree
ne_string label s =
  testCase label $ assertBool "non-empty string" $ length s > 0

-- just evaluating quasi quoters to HNF for now -- they
-- being tested everywhere [re|...|] (etc.) calculations
-- are bings used but HPC isn't measuring this
valid_res :: RegexType -> [QuasiQuoter] -> TestTree
valid_res rty = testCase (show rty) . foldr seq (return ())

pcre_prelude_macros :: [PreludeMacro]
pcre_prelude_macros = filter (/= PM_string) [minBound..maxBound]

tdfa_prelude_macros :: [PreludeMacro]
tdfa_prelude_macros = [minBound..maxBound]

s_toList :: S.Seq Char -> [Char]
s_toList = F.toList

newtype Identity a = Identity { runIdentity :: a }
  deriving (Functor)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  return = Identity
  (>>=) (Identity x) f = f x
\end{code}
