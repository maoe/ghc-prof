module GHC.Prof
  ( decode
  , decode'

  -- * Parser
  , profile

  -- * Cost-centre tree
  , CostCentreTree
  , aggregatedCostCentres
  , aggregatedCostCentresOrderBy
  , costCentres
  , costCentresOrderBy
  , aggregateCallSites
  , aggregateCallSitesOrderBy
  , callSites
  , callSitesOrderBy
  , aggregateModules
  , aggregateModulesOrderBy

  -- * Types
  , Profile(..)
  , TotalTime(..)
  , TotalAlloc(..)
  , AggregatedCostCentre(..)
  , CostCentre(..)
  , CostCentreNo
  , CallSite(..)
  , AggregateModule(..)
  ) where

import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.Attoparsec.Text as ATS
import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS

import GHC.Prof.CostCentreTree
import GHC.Prof.Parser (profile)
import GHC.Prof.Types

-- | Decode a GHC time allocation profiling report from a lazy 'ATL.Text'
decode :: TL.Text -> Either String Profile
decode text = case ATL.parse profile text of
  ATL.Fail _unconsumed _contexts reason -> Left reason
  ATL.Done _unconsumed prof -> Right prof

-- | Decode a GHC time allocation profiling report from a strict 'ATS.Text'
decode' :: TS.Text -> Either String Profile
decode' text = ATS.parseOnly (profile <* ATS.endOfInput) text
