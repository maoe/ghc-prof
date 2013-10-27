module GHC.RTS.TimeAllocProfile
  ( module GHC.RTS.TimeAllocProfile.Types
  , timeAllocProfile
  , profileCostCentres
  , profileCallSites
  ) where
import GHC.RTS.TimeAllocProfile.Parser (timeAllocProfile)
import GHC.RTS.TimeAllocProfile.Types
import GHC.RTS.TimeAllocProfile.CostCentre
