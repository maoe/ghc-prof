module GHC.RTS.TimeAllocProfile.Types where
import Data.Ratio (Ratio)
import Data.Text (Text)
import Data.Time (DiffTime, LocalTime)
import Data.Tree (Tree)

data TimeAllocProfile = TimeAllocProfile
  { profileTimestamp :: LocalTime
  , profileCommandLine :: Text
  , profileTotalTime :: TotalTime
  , profileTotalAlloc :: TotalAlloc
  , profileHotCostCentres :: [BriefCostCentre]
  , profileCostCentres :: Tree CostCentre
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
  , costCentreNo :: Int
  , costCentreEntries :: Integer
  , costCentreIndTime :: Double
  , costCentreIndAlloc :: Double
  , costCentreInhTime :: Double
  , costCentreInhAlloc :: Double
  , costCentreTicks :: Maybe Integer
  , costCentreBytes :: Maybe Integer
  } deriving Show
