{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Text.RE.Parsers
  ( parseInteger
  , parseHex
  , parseDouble
  , parseString
  , parseSimpleString
  , parseDate
  , parseSlashesDate
  , parseTimeOfDay
  , parseTimeZone
  , parseDateTime
  , parseDateTime8601
  , parseDateTimeCLF
  , parseShortMonth
  , shortMonthArray
  , IPV4Address
  , parseIPv4Address
  , Severity(..)
  , parseSeverity
  , severityKeywords
  ) where

import           Data.Array
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
import           Data.Time
import qualified Data.Time.Locale.Compat    as LC
import           Data.Word
import           Text.Printf
import           Text.Read
import           Text.RE.Replace


parseInteger :: Replace a => a -> Maybe Int
parseInteger = readMaybe . unpack_

parseHex :: Replace a => a -> Maybe Int
parseHex = readMaybe . ("0x"++) . unpack_

parseDouble :: Replace a => a -> Maybe Double
parseDouble = readMaybe . unpack_

parseString :: Replace a => a -> Maybe String
parseString = readMaybe . unpack_

parseSimpleString :: Replace a => a -> Maybe String
parseSimpleString = topntail . unpack_
  where
    topntail  ('"':t) = topntail' $ reverse t
    topntail  _       = Nothing

    topntail' ('"':rt) = Just $ reverse rt
    topntail' _        = Nothing

date_templates, time_templates, timezone_templates,
  date_time_8601_templates, date_time_templates :: [String]
date_templates            = ["%F"]
time_templates            = ["%H:%M:%S","%H:%M:%S%Q","%H:%M"]
timezone_templates        = ["Z","%z"]
date_time_8601_templates  =
    [ printf "%sT%s%s" dt tm tz
        | dt <- date_templates
        , tm <- time_templates
        , tz <- timezone_templates
        ]
date_time_templates       =
    [ printf "%s%c%s%s" dt sc tm tz
        | dt <- date_templates
        , sc <- ['T',' ']
        , tm <- time_templates
        , tz <- timezone_templates ++ [" UTC",""]
        ]

parseDate :: Replace a => a -> Maybe Day
parseDate = parse_time date_templates

parseSlashesDate :: Replace a => a -> Maybe Day
parseSlashesDate = parse_time ["%Y/%m/%d"]

parseTimeOfDay :: Replace a => a -> Maybe TimeOfDay
parseTimeOfDay = parse_time time_templates

parseTimeZone :: Replace a => a -> Maybe TimeZone
parseTimeZone = parse_time timezone_templates

parseDateTime :: Replace a => a -> Maybe UTCTime
parseDateTime = parse_time date_time_templates

parseDateTime8601 :: Replace a => a -> Maybe UTCTime
parseDateTime8601 = parse_time date_time_8601_templates

parseDateTimeCLF :: Replace a => a -> Maybe UTCTime
parseDateTimeCLF = parse_time ["%d/%b/%Y:%H:%M:%S %z"]

parseShortMonth :: Replace a => a -> Maybe Int
parseShortMonth = flip HM.lookup short_month_hm . unpack_

parse_time :: (ParseTime t,Replace s) => [String] -> s -> Maybe t
parse_time tpls = prs . unpack_
  where
    prs s = listToMaybe $ catMaybes
      [ parseTime LC.defaultTimeLocale fmt s
          | fmt<-tpls
          ]

short_month_hm :: HM.HashMap String Int
short_month_hm = HM.fromList [ (shortMonthArray!i,i) | i<-[1..12] ]

shortMonthArray :: Array Int String
shortMonthArray = listArray (1,12) $
  words "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"

type IPV4Address = (Word8,Word8,Word8,Word8)

parseIPv4Address :: Replace a => a -> Maybe IPV4Address
parseIPv4Address = prs . words_by (=='.') . unpack_
  where
    prs [a_s,b_s,c_s,d_s] = do
      a <- readMaybe a_s
      b <- readMaybe b_s
      c <- readMaybe c_s
      d <- readMaybe d_s
      case all is_o [a,b,c,d] of
        True  -> Just (toEnum a,toEnum b,toEnum c,toEnum d)
        False -> Nothing
    prs _ = Nothing

    is_o x = 0 <= x && x <= 255

data Severity
  = Emerg
  | Alert
  | Crit
  | Err
  | Warning
  | Notice
  | Info
  | Debug
  deriving (Bounded,Enum,Ord,Eq,Show)

parseSeverity :: Replace a => a -> Maybe Severity
parseSeverity = flip HM.lookup severity_hm . unpack_

severity_hm :: HM.HashMap String Severity
severity_hm = HM.fromList
  [ (kw,pri)
      | pri<-[minBound..maxBound]
      , let (kw0,kws) = severityKeywords pri
      , kw <- kw0:kws
      ]

severityKeywords :: Severity -> (String,[String])
severityKeywords pri = case pri of
  Emerg     -> (,) "emerg"    ["panic"]
  Alert     -> (,) "alert"    []
  Crit      -> (,) "crit"     []
  Err       -> (,) "err"      ["error"]
  Warning   -> (,) "warning"  ["warn"]
  Notice    -> (,) "notice"   []
  Info      -> (,) "info"     []
  Debug     -> (,) "debug"    []

words_by :: (Char->Bool) -> String -> [String]
words_by f s = case dropWhile f s of
  "" -> []
  s' -> w : words_by f s''
        where
          (w, s'') = break f s'
