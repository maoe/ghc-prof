{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Text (Text)
import Data.Tree
import Data.Time

import Data.Attoparsec.Text as A
import qualified Data.Tree.Zipper as Z

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
  profileCostCentres <- costCentres; skipSpace
  endOfInput
  return TimeAllocProfile {..}

timestamp :: Parser LocalTime
timestamp = do
  parseDayOfTheWeek; skipSpace
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
  string "total time  ="; skipSpace
  elapsed <- rational
  string " secs"; skipSpace
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
      [ ((10^3)*) <$> decimal <* string " us"
      , ((10^6)*) <$> decimal <* string " ms"
      ]

totalAlloc :: Parser TotalAlloc
totalAlloc = do
  string "total alloc ="; skipSpace
  n <- groupedDecimal
  string " bytes"; skipSpace
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

costCentres :: Parser (Tree CostCentre)
costCentres = header *> skipSpace *> costCentreTree
  where
    header = count 2 $ A.takeWhile (not . isEndOfLine) <* skipSpace

costCentre :: Parser CostCentre
costCentre = do
  name <- A.takeWhile (not . isSpace); skipSpace
  modName <- A.takeWhile (not . isSpace); skipSpace
  number <- decimal; skipSpace
  entries <- decimal; skipSpace
  indTime <- double; skipSpace
  indAlloc <- double; skipSpace
  inhTime <- double; skipSpace
  inhAlloc <- double;
  optInfo <- optional optionalInfo
  return CostCentre
    { costCentreName = name
    , costCentreModule = modName
    , costCentreNo = number
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

costCentreTree :: Parser (Tree CostCentre)
costCentreTree = buildTree <$> costCentreMap >>= maybe empty pure
  where
    costCentreMap = nestedCostCentre `sepBy1` endOfLine
    nestedCostCentre = (,) <$> nestLevel <*> costCentre
    nestLevel = howMany space

type Zipper = Z.TreePos Z.Full
type Level = Int

buildTree :: [(Level, a)] -> Maybe (Tree a)
buildTree [] = Nothing
buildTree ((lvl, t):xs) = Z.toTree <$> snd (foldl' go (lvl, Just z) xs)
  where
    z = Z.fromTree $ Node t []
    go
      :: (Level, Maybe (Zipper a))
      -> (Level, a)
      -> (Level, Maybe (Zipper a))
    go (curLvl, mzipper) a@(lvl', x)
      | curLvl > lvl' = go (curLvl-1, mzipper >>= Z.parent) a
      | curLvl < lvl' = case mzipper >>= Z.lastChild of
          Nothing  -> (lvl', Z.insert (Node x []) . Z.children <$> mzipper)
          mzipper' -> go (curLvl+1, mzipper') a
      | otherwise = (lvl', Z.insert (Node x []) . Z.nextSpace <$> mzipper)

howMany :: Parser a -> Parser Int
howMany p = loop 0
  where
    loop !n = (p >> loop (succ n)) <|> return n

parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"

symbol :: Parser Text
symbol = A.takeWhile $ not . isSpace
