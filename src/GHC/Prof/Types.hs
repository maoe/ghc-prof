{-# LANGUAGE RecordWildCards #-}
module GHC.Prof.Types where
import Data.Monoid
import Prelude

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (DiffTime, LocalTime)

-- | Top-level profiling report
data Profile = Profile
  { profileTimestamp :: !LocalTime
  , profileCommandLine :: !Text
  , profileTotalTime :: !TotalTime
  , profileTotalAlloc :: !TotalAlloc
  , profileTopCostCentres :: [AggregatedCostCentre]
  , profileCostCentreTree :: !CostCentreTree
  } deriving Show

-- | @total time@ in the profiling reports
data TotalTime = TotalTime
  { totalTimeElapsed :: !DiffTime
  -- ^ Total elapsed time in seconds
  , totalTimeTicks :: !Integer
  -- ^ Total number of ticks
  , totalTimeResolution :: !DiffTime
  -- ^ Duration of a tick
  , totalTimeProcessors :: !(Maybe Int)
  -- ^ Number of processors
  } deriving Show

-- | @total alloc@ in the profiling reports
newtype TotalAlloc = TotalAlloc
  { totalAllocBytes :: Integer
  -- ^ Total memory allocation in bytes
  } deriving Show

data AggregatedCostCentre = AggregatedCostCentre
  { aggregatedCostCentreName :: !Text
  -- ^ Name of the cost-centre
  , aggregatedCostCentreModule :: !Text
  -- ^ Module name of the cost-centre
  , aggregatedCostCentreSrc :: !(Maybe Text)
  -- ^ Source location of the cost-centre
  , aggregatedCostCentreEntries :: !(Maybe Integer)
  -- ^ Number of entries to the cost-centre
  , aggregatedCostCentreTime :: !Scientific
  -- ^ Total time spent in the cost-centre
  , aggregatedCostCentreAlloc :: !Scientific
  -- ^ Total allocation in the cost-centre
  , aggregatedCostCentreTicks :: !(Maybe Integer)
  -- ^ Total ticks in the cost-centre. This number exists only if
  -- @-P@ or @-Pa@ option is given at run-time.
  , aggregatedCostCentreBytes :: !(Maybe Integer)
  -- ^ Total memory allocation in the cost-centre. This number
  -- exists only if @-P@ or @-Pa@ option is given at run-time.
  } deriving (Show, Eq, Ord)

-- | Cost-centre node
data CostCentre = CostCentre
  { costCentreNo :: !CostCentreNo
  -- ^ Identifier of the cost-centre
  , costCentreName :: !Text
  -- ^ Name of the cost-centre
  , costCentreModule :: !Text
  -- ^ Module name of the cost-centre
  , costCentreSrc :: !(Maybe Text)
  -- ^ Source location of the cost-centre
  , costCentreEntries :: !Integer
  -- ^ Number of entries to the cost-centre
  , costCentreIndTime :: !Scientific
  -- ^ Time spent in the cost-centre itself
  , costCentreIndAlloc :: !Scientific
  -- ^ Allocation incurred by the cost-centre itself
  , costCentreInhTime :: !Scientific
  -- ^ Time spent in the cost-centre's children
  , costCentreInhAlloc :: !Scientific
  -- ^ Allocation incurred by the cost-centre's children
  , costCentreTicks :: !(Maybe Integer)
  -- ^ Number of ticks in the cost-centre.
  , costCentreBytes :: !(Maybe Integer)
  -- ^ Number of allocated bytes in the cost-centre.
  } deriving (Show, Eq, Ord)

type CostCentreNo = Int

data CostCentreTree = CostCentreTree
  { costCentreNodes :: !(IntMap CostCentre)
  , costCentreParents :: !(IntMap CostCentreNo)
  , costCentreChildren :: !(IntMap (Set CostCentre))
  , costCentreCallSites :: !(Map (Text, Text) (Set CostCentre))
  , costCentreAggregate :: !(Map Text (Map Text AggregatedCostCentre))
  } deriving Show

emptyCostCentreTree :: CostCentreTree
emptyCostCentreTree = CostCentreTree
  { costCentreNodes = mempty
  , costCentreParents = mempty
  , costCentreChildren = mempty
  , costCentreCallSites = mempty
  , costCentreAggregate = mempty
  }

data CallSite cc = CallSite
  { callSiteCostCentre :: cc
  -- ^ Metrics for the caller function
  , callSiteContribEntries :: !Integer
  -- ^ Number of entries contriubted by the caller function
  , callSiteContribTime :: !Scientific
  -- ^ Time contributed by the caller function
  , callSiteContribAlloc :: !Scientific
  -- ^ Allocation contributed by the caller function
  , callSiteContribTicks :: !(Maybe Integer)
  -- ^ Number of tikcs contributed by the caller function
  , callSiteContribBytes :: !(Maybe Integer)
  -- ^ Number of allocated bytes contributed byt hte caller function
  } deriving Show

data AggregateModule = AggregateModule
  { aggregateModuleName :: !Text
  -- ^ Name of the module
  , aggregateModuleEntries :: !(Maybe Integer)
  -- ^ Total number of entries to cost centres in the module
  , aggregateModuleTime :: !Scientific
  -- ^ Total time spent on cost centres in the module
  , aggregateModuleAlloc :: !Scientific
  -- ^ Total allocation on cost centres in the module
  , aggregateModuleTicks :: !(Maybe Integer)
  -- ^ Total ticks on cost centres in the module. This number exists only if
  -- @-P@ or @-Pa@ option is given at run-time.
  , aggregateModuleBytes :: !(Maybe Integer)
  -- ^ Total memory allocation on cost centres in the module. This number
  -- exists only if @-P@ or @-Pa@ option is given at run-time.
  } deriving (Show, Eq, Ord)

emptyAggregateModule :: Text -> AggregateModule
emptyAggregateModule name = AggregateModule
  { aggregateModuleName = name
  , aggregateModuleEntries = Just 0
  , aggregateModuleTime = 0
  , aggregateModuleAlloc = 0
  , aggregateModuleTicks = Just 0
  , aggregateModuleBytes = Just 0
  }
