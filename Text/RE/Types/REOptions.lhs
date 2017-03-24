\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Text.RE.Types.REOptions where

import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.String
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
\end{code}

\begin{code}
data REOptions_ r c e =
  REOptions
    { optionsMacs :: !(Macros r)
    , optionsComp :: !c
    , optionsExec :: !e
    }
  deriving (Show)
\end{code}

\begin{code}
class IsOption o r c e |
    e -> r, c -> e , e -> c, r -> c, c -> r, r -> e where
  makeREOptions :: o -> REOptions_ r c e
\end{code}

\begin{code}
newtype MacroID =
    MacroID { getMacroID :: String }
  deriving (IsString,Ord,Eq,Show)
\end{code}

\begin{code}
instance Hashable MacroID where
  hashWithSalt i = hashWithSalt i . getMacroID
\end{code}

\begin{code}
type Macros r = HM.HashMap MacroID r
\end{code}

\begin{code}
emptyMacros :: Macros r
emptyMacros = HM.empty
\end{code}

\begin{code}
data SimpleREOptions
  = MultilineSensitive
  | MultilineInsensitive
  | BlockSensitive
  | BlockInsensitive
  deriving (Bounded,Enum,Eq,Ord,Show)
\end{code}

\begin{code}
instance Lift SimpleREOptions where
  lift sro = case sro of
    MultilineSensitive    -> conE 'MultilineSensitive
    MultilineInsensitive  -> conE 'MultilineInsensitive
    BlockSensitive        -> conE 'BlockSensitive
    BlockInsensitive      -> conE 'BlockInsensitive
\end{code}
