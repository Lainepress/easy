\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}

module Main (main) where

import           Control.Exception
import qualified Data.ByteString.Lazy.Char8               as LBS
import qualified Data.Text                                as T
import           Prelude.Compat
import qualified Shelly                                   as SH
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           Text.RE.TDFA.ByteString.Lazy


main :: IO ()
main = do
  (pn,as) <- (,) <$> getProgName <*> getArgs
  case as of
    []        -> test
    ["test"]  -> test
    ["gen"]   -> gen
    _         -> do
      hPutStrLn stderr $ "usage: " ++ pn ++ " [test|gen]"
      exitWith $ ExitFailure 1

test :: IO ()
test = do
  createDirectoryIfMissing False "tmp"
  tdfa_ok <- and <$> mapM test' tdfa_edits
  pcre_ok <- and <$> mapM test' pcre_edits
  case tdfa_ok && pcre_ok of
    True  -> return ()
    False -> exitWith $ ExitFailure 1

test' :: (ModPath,SedScript RE) -> IO Bool
test' (mp,scr) = do
    putStrLn mp
    sed scr (mod_filepath source_mp) tmp_pth
    cmp     (T.pack tmp_pth) (T.pack $ mod_filepath mp)
  where
    tmp_pth = "tmp/prog.hs"

gen :: IO ()
gen = do
  mapM_ gen' tdfa_edits
  mapM_ gen' pcre_edits

gen' :: (ModPath,SedScript RE) -> IO ()
gen' (mp,scr) = do
  putStrLn mp
  sed scr (mod_filepath source_mp) (mod_filepath mp)

tdfa_edits :: [(ModPath,SedScript RE)]
tdfa_edits =
  [ tdfa_edit "Text.RE.TDFA.ByteString"       "B.ByteString"    "import qualified Data.ByteString               as B"
  , tdfa_edit "Text.RE.TDFA.Sequence"         "(S.Seq Char)"    "import qualified Data.Sequence                 as S"
  , tdfa_edit "Text.RE.TDFA.String"           "String"          ""
  , tdfa_edit "Text.RE.TDFA.Text"             "T.Text"          "import qualified Data.Text                     as T"
  , tdfa_edit "Text.RE.TDFA.Text.Lazy"        "TL.Text"         "import qualified Data.Text.Lazy                as TL"
  ]

pcre_edits :: [(ModPath,SedScript RE)]
pcre_edits =
  [ pcre_edit "Text.RE.PCRE.ByteString"       "B.ByteString"    "import qualified Data.ByteString               as B"
  , pcre_edit "Text.RE.PCRE.ByteString.Lazy"  "LBS.ByteString"  "import qualified Data.ByteString.Lazy          as LBS"
  , pcre_edit "Text.RE.PCRE.Sequence"         "(S.Seq Char)"    "import qualified Data.Sequence                 as S"
  , pcre_edit "Text.RE.PCRE.String"           "String"          ""
  ]

tdfa_edit :: ModPath
          -> LBS.ByteString
          -> LBS.ByteString
          -> (ModPath,SedScript RE)
tdfa_edit mp bs_lbs import_lbs =
    (,) mp $ Pipe
        [ (,) module_re $ EDIT_tpl $ LBS.pack mp
        , (,) import_re $ EDIT_tpl   import_lbs
        , (,) bs_re     $ EDIT_tpl   bs_lbs
        ]

pcre_edit :: ModPath
          -> LBS.ByteString
          -> LBS.ByteString
          -> (ModPath,SedScript RE)
pcre_edit mp bs_lbs import_lbs =
    (,) mp $ Pipe
        [ (,) tdfa_re   $ EDIT_tpl   "PCRE"
        , (,) module_re $ EDIT_tpl $ LBS.pack mp
        , (,) import_re $ EDIT_tpl   import_lbs
        , (,) bs_re     $ EDIT_tpl   bs_lbs
        ]

type ModPath = String

source_mp :: ModPath
source_mp = "Text.RE.TDFA.ByteString.Lazy"

tdfa_re, module_re, import_re, bs_re :: RE
tdfa_re   = [re|TDFA|]
module_re = [re|Text.RE.TDFA.ByteString.Lazy|]
import_re = [re|import qualified Data.ByteString.Lazy.Char8 *as LBS|]
bs_re     = [re|LBS.ByteString|]

mod_filepath :: ModPath -> FilePath
mod_filepath mp = map tr mp ++ ".hs"
  where
    tr '.' = '/'
    tr c   = c

cmp :: T.Text -> T.Text -> IO Bool
cmp src dst = handle hdl $ do
    _ <- SH.shelly $ SH.verbosely $
        SH.run "cmp" [src,dst]
    return True
  where
    hdl :: SomeException -> IO Bool
    hdl se = do
      hPutStrLn stderr $
        "testing results against model answers failed: " ++ show se
      return False
\end{code}
