module GHC.Prof
  ( decode

  -- * Parser
  , profile

  -- * Cost-centre tree
  , CostCentreTree
  , aggregateCostCentres
  , aggregateCostCentresOrderBy
  , costCentres
  , costCentresOrderBy
  , callSites
  , callSitesOrderBy

  -- * Types
  , Profile(..)
  , TotalTime(..)
  , TotalAlloc(..)
  , AggregateCostCentre(..)
  , CostCentre(..)
  , CostCentreNo
  , CallSite(..)
  ) where

import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.Text.Lazy as TL

import GHC.Prof.CostCentreTree
import GHC.Prof.Parser (profile)
import GHC.Prof.Types

-- | Decode a GHC time allocation profiling report from a lazy 'ATL.Text'
decode :: TL.Text -> Either String Profile
decode text = case ATL.parse profile text of
  ATL.Fail _unconsumed _contexts reason -> Left reason
  ATL.Done _unconsumed prof -> Right prof
