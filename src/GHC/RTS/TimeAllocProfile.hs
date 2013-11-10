module GHC.RTS.TimeAllocProfile
  ( TimeAllocProfile(..)
  , TotalTime(..)
  , TotalAlloc(..)
  , BriefCostCentre(..)
  , CostCentre(..)
  , CostCentreNo
  , Callee(..)
  , CallSite(..)

  -- * Parser
  , timeAllocProfile

  -- * Cost-centre tree
  , CostCentreTree
  , profileCostCentres
  , profileCostCentresOrderBy
  , profileCallSites
  , profileCallSitesOrderBy
  ) where

import GHC.RTS.TimeAllocProfile.CostCentreTree
import GHC.RTS.TimeAllocProfile.Parser (timeAllocProfile)
import GHC.RTS.TimeAllocProfile.Types
