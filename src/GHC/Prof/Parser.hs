{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GHC.Prof.Parser
  ( timeAllocProfile

  , timestamp
  , title
  , commandLine
  , totalTime
  , totalAlloc
  , hotCostCentres
  , briefCostCentre
  , costCentres
  , costCentre
  ) where
import Control.Applicative
import Control.Monad
import Data.Char (isSpace)
import Data.Foldable (asum, foldl')
import Data.Sequence (Seq, (><), (|>))
import Data.Text (Text)
import Data.Time
import qualified Data.Sequence as Seq

import Data.Attoparsec.Text as A

import GHC.Prof.Types

#if MIN_VERSION_containers(0, 5, 0)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
#else
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
#endif

timeAllocProfile :: Parser TimeAllocProfile
timeAllocProfile = do
  skipHorizontalSpace
  profileTimestamp <- timestamp; skipSpace
  void title; skipSpace
  profileCommandLine <- commandLine; skipSpace
  profileTotalTime <- totalTime; skipSpace
  profileTotalAlloc <- totalAlloc; skipSpace
  profileHotCostCentres <- hotCostCentres; skipSpace
  profileCostCentreTree <- costCentres; skipSpace
  endOfInput
  return $! TimeAllocProfile {..}

timestamp :: Parser LocalTime
timestamp = do
  parseDayOfTheWeek >> skipSpace
  month <- parseMonth; skipSpace
  day <- parseDay; skipSpace
  tod <- parseTimeOfDay; skipSpace
  year <- parseYear; skipSpace
  return $! LocalTime
    { localDay = fromGregorian year month day
    , localTimeOfDay = tod
    }
  where
    parseYear = decimal
    parseMonth = A.take 3 >>= nameToInt
      where
        nameToInt name = case name of
          "Jan" -> return 1; "Feb" -> return 2; "Mar" -> return 3
          "Apr" -> return 4; "May" -> return 5; "Jun" -> return 6
          "Jul" -> return 7; "Aug" -> return 8; "Sep" -> return 9
          "Oct" -> return 10; "Nov" -> return 11; "Dec" -> return 12
          _ -> fail $ "timestamp.toNum: invalid month - " ++ show name
    parseDay = decimal
    parseTimeOfDay = TimeOfDay
      <$> decimal <* string ":"
      <*> decimal
      <*> pure 0
    parseDayOfTheWeek = takeTill isSpace

title :: Parser Text
title = string "Time and Allocation Profiling Report  (Final)"

commandLine :: Parser Text
commandLine = A.takeWhile $ not . isEndOfLine

totalTime :: Parser TotalTime
totalTime = do
  void $ string "total time  ="; skipSpace
  elapsed <- rational
  void $ string " secs"; skipSpace
  (ticks, resolution, processors) <- parens $ (,,)
    <$> decimal <* string " ticks @ "
    <*> picoSeconds <* string ", "
    <*> decimal <* many1 (notChar ')')
  return $! TotalTime
    { totalTimeElapsed = elapsed
    , totalTimeTicks = ticks
    , totalTimeResolution = picosecondsToDiffTime resolution
    , totalTimeProcessors = processors
    }
  where
    picoSeconds = asum
      [ ((10 `pow` 3)*) <$> decimal <* string " us"
      , ((10 `pow` 6)*) <$> decimal <* string " ms"
      ]
    pow :: Integer -> Int -> Integer
    pow = (^)

totalAlloc :: Parser TotalAlloc
totalAlloc = do
  string "total alloc =" >> skipSpace
  !n <- groupedDecimal
  string " bytes" >> skipSpace
  parens $ void $ string "excludes profiling overheads"
  return TotalAlloc { totalAllocBytes = n }
  where
    groupedDecimal = do
      ds <- decimal `sepBy` char ','
      return $! foldl' go 0 ds
      where
        go z n = z * 1000 + n

newtype HeaderParams = HeaderParams
  { headerHasSrc :: Bool -- ^ SRC column exists
  } deriving Show

header :: Parser HeaderParams
header = do
  optional_ $ do
    string "individual" >> skipHorizontalSpace
    string "inherited" >> skipSpace
  string "COST CENTRE" >> skipHorizontalSpace
  string "MODULE" >> skipHorizontalSpace
  headerHasSrc <- option False $ True <$ string "SRC"; skipHorizontalSpace
  optional_ $ string "no." >> skipHorizontalSpace
  optional_ $ string "entries" >> skipHorizontalSpace
  string "%time" >> skipHorizontalSpace
  string "%alloc" >> skipHorizontalSpace
  optional_ $ do
    string "%time" >> skipHorizontalSpace
    string "%alloc" >> skipHorizontalSpace
  optional_ $ do
    string "ticks" >> skipHorizontalSpace
    string "bytes" >> skipHorizontalSpace
  return HeaderParams
    {..}

hotCostCentres :: Parser [BriefCostCentre]
hotCostCentres = do
  params <- header; skipSpace
  briefCostCentre params `sepBy1` endOfLine

briefCostCentre :: HeaderParams -> Parser BriefCostCentre
briefCostCentre HeaderParams {..} = BriefCostCentre
  <$> symbol <* skipHorizontalSpace -- name
  <*> symbol <* skipHorizontalSpace -- module
  <*> source <* skipHorizontalSpace -- src
  <*> double <* skipHorizontalSpace -- %time
  <*> double <* skipHorizontalSpace -- %alloc
  <*> optional decimal <* skipHorizontalSpace -- ticks
  <*> optional decimal <* skipHorizontalSpace -- bytes
  where
    source
      | headerHasSrc = Just <$> symbol
      | otherwise = pure Nothing

costCentres :: Parser CostCentreTree
costCentres = do
  params <- header; skipSpace
  costCentreTree params

costCentre :: HeaderParams -> Parser CostCentre
costCentre HeaderParams {..} = do
  name <- symbol; skipHorizontalSpace
  modName <- symbol; skipHorizontalSpace
  src <- if headerHasSrc
    then do
      !sym <- symbol
      return $! Just sym
    else pure Nothing
  skipHorizontalSpace
  no <- decimal; skipHorizontalSpace
  entries <- decimal; skipHorizontalSpace
  indTime <- double; skipHorizontalSpace
  indAlloc <- double; skipHorizontalSpace
  inhTime <- double; skipHorizontalSpace
  inhAlloc <- double; skipHorizontalSpace
  optInfo <- optional optionalInfo
  return $! CostCentre
    { costCentreName = name
    , costCentreModule = modName
    , costCentreSrc = src
    , costCentreNo = no
    , costCentreEntries = entries
    , costCentreIndTime = indTime
    , costCentreIndAlloc = indAlloc
    , costCentreInhTime = inhTime
    , costCentreInhAlloc = inhAlloc
    , costCentreTicks = fst <$> optInfo
    , costCentreBytes = snd <$> optInfo
    }
  where
    optionalInfo = do
      !ticks <- decimal
      skipHorizontalSpace
      !bytes <- decimal
      return (ticks, bytes)

costCentreTree :: HeaderParams -> Parser CostCentreTree
costCentreTree params = buildTree <$> costCentreList
  where
    costCentreList = nestedCostCentre `sepBy1` endOfLine
    nestedCostCentre = (,)
      <$> nestLevel
      <*> costCentre params
      <* skipHorizontalSpace
    nestLevel = howMany space

type Level = Int
type TreePath = Seq Level

buildTree :: [(Level, CostCentre)] -> CostCentreTree
buildTree = snd . foldl' go (Seq.empty, emptyCostCentreTree)
  where
    go
      :: (TreePath, CostCentreTree)
      -> (Level, CostCentre)
      -> (TreePath, CostCentreTree)
    go (treePath, tree) (level, node) = (treePath', tree')
      where
        !treePath' = Seq.take level treePath |> costCentreNo node
        !tree' = if Seq.length treePath == 0
          then CostCentreTree
            { costCentreNodes = IntMap.singleton nodeNo node
            , costCentreParents = IntMap.empty
            , costCentreChildren = IntMap.empty
            , costCentreCallSites = Map.singleton
                (costCentreName node, costCentreModule node)
                Seq.empty
            }
          else CostCentreTree
            { costCentreNodes = IntMap.insert nodeNo node
                (costCentreNodes tree)
            , costCentreParents = IntMap.insert nodeNo parent
                (costCentreParents tree)
            , costCentreChildren = IntMap.insertWith (><)
                parent
                (Seq.singleton node)
                (costCentreChildren tree)
            , costCentreCallSites = Map.insertWith (><)
                (costCentreName node, costCentreModule node)
                (Seq.singleton node)
                (costCentreCallSites tree)
            }
          where
            nodeNo = costCentreNo node
            parent = Seq.index treePath (level - 1)

howMany :: Parser a -> Parser Int
howMany p = loop 0
  where
    loop !n = (p >> loop (succ n)) <|> return n

parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"

symbol :: Parser Text
symbol = A.takeWhile $ not . isSpace

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = void $ A.takeWhile isHorizontalSpace

optional_ :: Parser a -> Parser ()
optional_ = void . optional
