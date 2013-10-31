{-# LANGUAGE RecordWildCards #-}
module GHC.RTS.TimeAllocProfile.Types where
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Time (DiffTime, LocalTime)
import Data.Sequence (Seq)

data TimeAllocProfile = TimeAllocProfile
  { profileTimestamp :: LocalTime
  , profileCommandLine :: Text
  , profileTotalTime :: TotalTime
  , profileTotalAlloc :: TotalAlloc
  , profileHotCostCentres :: [BriefCostCentre]
  , profileCostCentreTree :: CostCentreTree
  } deriving Show

data TotalTime = TotalTime
  { totalTimeElapsed :: DiffTime
  , totalTimeTicks :: Integer
  , totalTimeResolution :: DiffTime
  , totalTimeProcessors :: Int
  } deriving Show

data TotalAlloc = TotalAlloc
  { totalAllocBytes :: Integer
  } deriving Show

data BriefCostCentre = BriefCostCentre
  { briefCostCentreName :: Text
  , briefCostCentreModule :: Text
  , briefCostCentreTime :: Double
  , briefCostCentreAlloc :: Double
  , briefCostCentreTicks :: Maybe Integer
  , briefCostCentreBytes :: Maybe Integer
  } deriving Show

data CostCentre = CostCentre
  { costCentreName :: Text
  , costCentreModule :: Text
  , costCentreNo :: CostCentreNo
  , costCentreEntries :: Integer
  , costCentreIndTime :: Double
  , costCentreIndAlloc :: Double
  , costCentreInhTime :: Double
  , costCentreInhAlloc :: Double
  , costCentreTicks :: Maybe Integer
  , costCentreBytes :: Maybe Integer
  } deriving Show

type CostCentreNo = Int

data CostCentreTree = CostCentreTree
  { costCentreNodes :: !(IntMap CostCentre)
  , costCentreParents :: !(IntMap CostCentreNo)
  , costCentreChildren :: !(IntMap (Seq CostCentre))
  , costCentreCallSites :: !(Map (Text, Text) (Seq CostCentre))
  } deriving Show

emptyCostCentreTree :: CostCentreTree
emptyCostCentreTree = CostCentreTree
  { costCentreNodes = mempty
  , costCentreParents = mempty
  , costCentreChildren = mempty
  , costCentreCallSites = mempty
  }

data Callee = Callee
  { calleeName :: Text
  , calleeModule :: Text
  , calleeEntries :: Integer
  , calleeTime :: Double
  , calleeAlloc :: Double
  , calleeTicks :: Maybe Integer
  , calleeBytes :: Maybe Integer
  } deriving Show

data CallSite = CallSite
  { callSiteCostCentre :: CostCentre
  , callSiteContribEntries :: !Integer
  , callSiteContribTime :: !Double
  , callSiteContribAlloc :: !Double
  , callSiteContribTicks :: !(Maybe Integer)
  , callSiteContribBytes :: !(Maybe Integer)
  } deriving Show
