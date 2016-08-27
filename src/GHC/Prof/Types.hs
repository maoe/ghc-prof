{-# LANGUAGE RecordWildCards #-}
module GHC.Prof.Types where
import Data.Monoid
import Prelude

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (DiffTime, LocalTime)

-- | Top-level profiling report
data Profile = Profile
  { profileTimestamp :: !LocalTime
  , profileCommandLine :: !Text
  , profileTotalTime :: !TotalTime
  , profileTotalAlloc :: !TotalAlloc
  , profileTopCostCentres :: [AggregateCostCentre]
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
  , totalTimeProcessors :: !Int
  -- ^ Number of processors
  } deriving Show

-- | @total alloc@ in the profiling reports
newtype TotalAlloc = TotalAlloc
  { totalAllocBytes :: Integer
  -- ^ Total memory allocation in bytes
  } deriving Show

data AggregateCostCentre = AggregateCostCentre
  { aggregateCostCentreName :: !Text
  -- ^ Name of the cost-centre
  , aggregateCostCentreModule :: !Text
  -- ^ Module name of the cost-centre
  , aggregateCostCentreSrc :: !(Maybe Text)
  -- ^ Source location of the cost-centre
  , aggregateCostCentreTime :: !Scientific
  -- ^ Total time spent in the cost-centre
  , aggregateCostCentreAlloc :: !Scientific
  -- ^ Total allocation in the cost-centre
  , aggregateCostCentreTicks :: !(Maybe Integer)
  -- ^ Total ticks in the cost-centre. This number exists only if
  -- @-P@ or @-Pa@ option is given at run-time.
  , aggregateCostCentreBytes :: !(Maybe Integer)
  -- ^ Total memory allocation in the cost-centre. This number
  -- exists only if @-P@ or @-Pa@ option is given at run-time.
  } deriving Show

-- | Cost-centre node
data CostCentre = CostCentre
  { costCentreName :: !Text
  -- ^ Name of the cost-centre
  , costCentreModule :: !Text
  -- ^ Module name of the cost-centre
  , costCentreSrc :: !(Maybe Text)
  -- ^ Source location of the cost-centre
  , costCentreNo :: !CostCentreNo
  -- ^ Identifier of the cost-centre
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
  } deriving Show

type CostCentreNo = Int

data CostCentreTree = CostCentreTree
  { costCentreNodes :: !(IntMap CostCentre)
  , costCentreParents :: !(IntMap CostCentreNo)
  , costCentreChildren :: !(IntMap (Seq CostCentre))
  , costCentreCallSites :: !(Map (Text, Text) (Seq CostCentre))
  , costCentreAggregate :: !(Map (Text, Text) AggregateCostCentre)
  } deriving Show

emptyCostCentreTree :: CostCentreTree
emptyCostCentreTree = CostCentreTree
  { costCentreNodes = mempty
  , costCentreParents = mempty
  , costCentreChildren = mempty
  , costCentreCallSites = mempty
  , costCentreAggregate = mempty
  }

data Callee = Callee
  { calleeName :: Text
  -- ^ Name of the callee function
  , calleeModule :: Text
  -- ^ Module name of the calle function
  , calleeEntries :: !Integer
  -- ^ Number of entries to the callee function
  , calleeTime :: !Scientific
  -- ^ Time spent in the callee function
  , calleeAlloc :: !Scientific
  -- ^ Allocation incurred by the callee function
  , calleeTicks :: !(Maybe Integer)
  -- ^ Number of ticks in the callee function
  , calleeBytes :: !(Maybe Integer)
  -- ^ Number of allocated bytes in the callee function
  } deriving Show

data CallSite = CallSite
  { callSiteCostCentre :: CostCentre
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
