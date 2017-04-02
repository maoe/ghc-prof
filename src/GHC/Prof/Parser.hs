{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module GHC.Prof.Parser
  ( profile

  , timestamp
  , title
  , commandLine
  , totalTime
  , totalAlloc
  , topCostCentres
  , aggregatedCostCentre
  , costCentres
  , costCentre
  ) where
import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace)
import Data.Foldable (asum, foldl')
import Data.Maybe
import Data.Time

import Data.Text (Text)
import Data.Attoparsec.Text as A
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Control.Monad.Extras (seqM)
import GHC.Prof.Types

#if MIN_VERSION_containers(0, 5, 0)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
#else
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
#endif

-- | Parse a GHC time-allocation profiling report
profile :: Parser Profile
profile = do
  skipHorizontalSpace
  profileTimestamp <- timestamp; skipSpace
  void title; skipSpace
  profileCommandLine <- commandLine; skipSpace
  profileTotalTime <- totalTime; skipSpace
  profileTotalAlloc <- totalAlloc; skipSpace
  profileTopCostCentres <- topCostCentres; skipSpace
  profileCostCentreTree <- costCentres; skipSpace
  endOfInput
  return $! Profile {..}

-- | Parse the timestamp in a header as local time
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
    <*> picoSeconds
    <*> optional (string ", " *> decimal <* many1 (notChar ')'))
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

topCostCentres :: Parser [AggregatedCostCentre]
topCostCentres = do
  params <- header; skipSpace
  aggregatedCostCentre params `sepBy1` endOfLine

aggregatedCostCentre :: HeaderParams -> Parser AggregatedCostCentre
aggregatedCostCentre HeaderParams {..} = AggregatedCostCentre
  <$> symbol <* skipHorizontalSpace -- name
  <*> symbol <* skipHorizontalSpace -- module
  <*> source <* skipHorizontalSpace -- src
  <*> pure Nothing -- entries
  <*> scientific <* skipHorizontalSpace -- %time
  <*> scientific <* skipHorizontalSpace -- %alloc
  <*> optional decimal <* skipHorizontalSpace -- ticks
  <*> optional decimal <* skipHorizontalSpace -- bytes
  where
    source
      | headerHasSrc = Just <$> sourceSpan
      | otherwise = pure Nothing

costCentres :: Parser CostCentreTree
costCentres = do
  params <- header; skipSpace
  costCentreTree params

costCentre :: HeaderParams -> Parser CostCentre
costCentre params = do
  name <- symbol; skipHorizontalSpace
  (modName, src, no, (entries, indTime, indAlloc, inhTime, inhAlloc, optInfo))
    <- validCostCentre params <|> jammedCostCentre
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
    validCostCentre HeaderParams {..} = do
      modName <- symbol; skipHorizontalSpace
      src <- if headerHasSrc
        then do
          !sym <- sourceSpan
          return $ Just sym
        else pure Nothing
      skipHorizontalSpace
      no <- decimal; skipHorizontalSpace
      vals <- metrics
      return (modName, src, no, vals)
    -- Workaround for https://ghc.haskell.org/trac/ghc/ticket/8811.
    -- This bug had been fixed before the SRC column was implemented so
    -- @sourceSpan@ isn't parsed here.
    -- Caveat: This parser can be confused if module name contains digits and
    -- the digits are jammed with the cost centre number. In such cases, all
    -- the digits are parsed as a number of entries.
    jammedCostCentre = do
      jammed <- symbol; skipHorizontalSpace
      let modName = T.dropWhileEnd isDigit jammed
      no <- either fail (return . fst) $ TR.decimal $ T.takeWhileEnd isDigit jammed
      vals <- metrics
      return (modName, Nothing, no, vals)
    metrics = do
      entries <- decimal; skipHorizontalSpace
      indTime <- scientific; skipHorizontalSpace
      indAlloc <- scientific; skipHorizontalSpace
      inhTime <- scientific; skipHorizontalSpace
      inhAlloc <- scientific; skipHorizontalSpace
      optInfo <- optional $ do
        !ticks <- decimal; skipHorizontalSpace
        !bytes <- decimal
        return (ticks, bytes)
      return (entries, indTime, indAlloc, inhTime, inhAlloc, optInfo)

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

-- | TreePath represents a path to a node in a cost centre tree.
--
-- Invariant: @'treePathLevel' == length 'treePath'@
data TreePath = TreePath
  { treePathLevel :: !Level
  -- ^ Current depth of the path
  , treePath :: [CostCentreNo]
  -- ^ Path to the node
  }

push :: CostCentreNo -> TreePath -> TreePath
push ccNo path@TreePath {..} = path
  { treePathLevel = treePathLevel + 1
  , treePath = ccNo:treePath
  }

popTo :: Level -> TreePath -> TreePath
popTo level path@TreePath {..} = path
  { treePathLevel = level
  , treePath = drop (treePathLevel - level) treePath
  }

currentNo :: TreePath -> Maybe CostCentreNo
currentNo TreePath {treePath} = listToMaybe treePath

buildTree :: [(Level, CostCentre)] -> CostCentreTree
buildTree = snd . foldl' go (TreePath 0 [], emptyCostCentreTree)
  where
    go
      :: (TreePath, CostCentreTree)
      -> (Level, CostCentre)
      -> (TreePath, CostCentreTree)
    go (!path, !CostCentreTree {..}) (level, node) = (path', tree')
      where
        ccNo = costCentreNo node
        parentPath = popTo level path
        parentNo = currentNo parentPath
        path' = push ccNo parentPath
        tree' = CostCentreTree
          { costCentreNodes = IntMap.insert ccNo node costCentreNodes
          , costCentreParents = maybe costCentreParents
            (\parent -> IntMap.insert ccNo parent costCentreParents)
            parentNo
          , costCentreChildren = maybe costCentreChildren
            (\parent -> IntMap.insertWith Set.union parent
              (Set.singleton node)
              costCentreChildren)
            parentNo
          , costCentreCallSites = Map.insertWith Set.union
            (costCentreName node, costCentreModule node)
            (Set.singleton node)
            costCentreCallSites
          , costCentreAggregate = Map.alter
            (Just . updateCostCentre)
            (costCentreModule node)
            costCentreAggregate
          }
        aggregate = AggregatedCostCentre
          { aggregatedCostCentreName = costCentreName node
          , aggregatedCostCentreModule = costCentreModule node
          , aggregatedCostCentreSrc = costCentreSrc node
          , aggregatedCostCentreEntries = Just $! costCentreEntries node
          , aggregatedCostCentreTime = costCentreIndTime node
          , aggregatedCostCentreAlloc = costCentreIndAlloc node
          , aggregatedCostCentreTicks = costCentreTicks node
          , aggregatedCostCentreBytes = costCentreBytes node
          }
        updateCostCentre
          :: Maybe (Map.Map Text AggregatedCostCentre)
          -> Map.Map Text AggregatedCostCentre
        updateCostCentre = \case
          Nothing -> Map.singleton (costCentreName node) aggregate
          Just costCentreByName ->
            Map.insertWith
              addCostCentre
              (costCentreName node)
              aggregate
              costCentreByName
        addCostCentre x y = x
          { aggregatedCostCentreEntries = seqM $ (+)
            <$> aggregatedCostCentreEntries x
            <*> aggregatedCostCentreEntries y
          , aggregatedCostCentreTime =
            aggregatedCostCentreTime x + aggregatedCostCentreTime y
          , aggregatedCostCentreAlloc =
            aggregatedCostCentreAlloc x + aggregatedCostCentreAlloc y
          , aggregatedCostCentreTicks = seqM $ (+)
            <$> aggregatedCostCentreTicks x
            <*> aggregatedCostCentreTicks y
          , aggregatedCostCentreBytes = seqM $ (+)
            <$> aggregatedCostCentreBytes x
            <*> aggregatedCostCentreBytes y
          }

howMany :: Parser a -> Parser Int
howMany p = loop 0
  where
    loop !n = (p >> loop (succ n)) <|> return n

parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"

symbol :: Parser Text
symbol = A.takeWhile $ not . isSpace

sourceSpan :: Parser Text
sourceSpan = asum
  [ T.pack <$> angleBrackets
  , symbol
  ]
  where
    angleBrackets = (:) <$> char '<' <*> manyTill anyChar (char '>')

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = void $ A.takeWhile isHorizontalSpace

optional_ :: Parser a -> Parser ()
optional_ = void . optional
