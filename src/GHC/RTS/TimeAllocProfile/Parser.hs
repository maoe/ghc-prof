{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module GHC.RTS.TimeAllocProfile.Parser
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
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Foldable (asum, foldl')
import Data.Sequence (Seq, (><), (|>))
import Data.Text (Text)
import Data.Time
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Data.Attoparsec.Text as A

import GHC.RTS.TimeAllocProfile.Types

timeAllocProfile :: Parser TimeAllocProfile
timeAllocProfile = do
  skipSpace
  profileTimestamp <- timestamp; skipSpace
  void title; skipSpace
  profileCommandLine <- commandLine; skipSpace
  profileTotalTime <- totalTime; skipSpace
  profileTotalAlloc <- totalAlloc; skipSpace
  profileHotCostCentres <- hotCostCentres; skipSpace
  profileCostCentreTree <- costCentres; skipSpace
  endOfInput
  return TimeAllocProfile {..}

timestamp :: Parser LocalTime
timestamp = do
  void parseDayOfTheWeek; skipSpace
  month <- parseMonth; skipSpace
  day <- parseDay; skipSpace
  tod <- parseTimeOfDay; skipSpace
  year <- parseYear; skipSpace
  return LocalTime
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
  return TotalTime
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
  void $ string "total alloc ="; skipSpace
  n <- groupedDecimal
  void $ string " bytes"; skipSpace
  parens $ void $ string "excludes profiling overheads"
  return TotalAlloc { totalAllocBytes = n }
  where
    groupedDecimal = foldl' go 0 <$> decimal `sepBy` char ','
      where
        go z n = z * 1000 + n

hotCostCentres :: Parser [BriefCostCentre]
hotCostCentres =
  header *> skipSpace *> many1 (briefCostCentre <* skipSpace)
  where
    header = A.takeWhile $ not . isEndOfLine

briefCostCentre :: Parser BriefCostCentre
briefCostCentre = BriefCostCentre
  <$> symbol <* skipSpace -- name
  <*> symbol <* skipSpace -- module
  <*> double <* skipSpace -- %time
  <*> double <* skipSpace -- %alloc
  <*> optional decimal <* skipSpace -- ticks
  <*> optional decimal -- bytes

costCentres :: Parser CostCentreTree
costCentres = header *> skipSpace *> costCentreTree
  where
    !header = count 2 $ A.takeWhile (not . isEndOfLine) <* skipSpace

costCentre :: Parser CostCentre
costCentre = do
  name <- A.takeWhile (not . isSpace); skipSpace
  modName <- A.takeWhile (not . isSpace); skipSpace
  no <- decimal; skipSpace
  entries <- decimal; skipSpace
  indTime <- double; skipSpace
  indAlloc <- double; skipSpace
  inhTime <- double; skipSpace
  inhAlloc <- double;
  optInfo <- optional optionalInfo
  return CostCentre
    { costCentreName = name
    , costCentreModule = modName
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
      skipSpace
      ticks <- decimal; skipSpace
      bytes <- decimal
      return (ticks, bytes)

costCentreTree :: Parser CostCentreTree
costCentreTree = buildTree <$> costCentreList
  where
    costCentreList = nestedCostCentre `sepBy1` endOfLine
    nestedCostCentre = (,) <$> nestLevel <*> costCentre
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
