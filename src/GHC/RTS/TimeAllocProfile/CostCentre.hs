{-# LANGUAGE RecordWildCards #-}
module GHC.RTS.TimeAllocProfile.CostCentre
  ( profileCostCentres
  , profileCostCentresOrderBy
  , profileCallSites

  , buildCostCentresOrderBy
  , buildCallSitesOrderBy
  ) where
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Data.Foldable (asum)
import Data.Function (on)
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Traversable (mapM)
import Data.Tree (Tree)
import Prelude hiding (mapM)
import qualified Data.Foldable as Fold
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Tree as Tree

import GHC.RTS.TimeAllocProfile.Types

profileCostCentres :: TimeAllocProfile -> Maybe (Tree CostCentre)
profileCostCentres = profileCostCentresOrderBy sortKey
  where
    sortKey =
      costCentreInhTime &&& costCentreIndTime &&&
      costCentreInhAlloc &&& costCentreIndAlloc

profileCostCentresOrderBy
  :: Ord a
  => (CostCentre -> a)
  -> TimeAllocProfile
  -> Maybe (Tree CostCentre)
profileCostCentresOrderBy sortKey =
  buildCostCentresOrderBy sortKey . profileCostCentreTree

profileCallSites
  :: Text
  -> Text
  -> TimeAllocProfile
  -> Maybe (Callee, Seq CallSite)
profileCallSites = profileCallSitesOrderBy sortKey
  where
    sortKey =
      costCentreInhTime &&& costCentreIndTime &&&
      costCentreInhAlloc &&& costCentreIndAlloc

profileCallSitesOrderBy
  :: Ord a
  => (CostCentre -> a)
  -> Text
  -> Text
  -> TimeAllocProfile
  -> Maybe (Callee, Seq CallSite)
profileCallSitesOrderBy sortKey name modName =
  buildCallSitesOrderBy sortKey name modName . profileCostCentreTree

buildCostCentresOrderBy
  :: Ord a
  => (CostCentre -> a)
  -> CostCentreTree
  -> Maybe (Tree CostCentre)
buildCostCentresOrderBy sortKey CostCentreTree {..} = do
  -- Invariant:
  --   The root node (MAIN.MAIN) should have the least cost centre ID
  rootKey <- listToMaybe $ IntMap.keys costCentreNodes
  Tree.unfoldTreeM build rootKey
  where
    build key = do
      node <- IntMap.lookup key costCentreNodes
      return (node, children)
      where
          children = maybe [] Fold.toList $ do
            nodes <- IntMap.lookup key costCentreChildren
            return $ costCentreNo
                <$> Seq.unstableSortBy (flip compare `on` sortKey) nodes

buildCallSitesOrderBy
  :: Ord a
  => (CostCentre -> a)
  -- ^ Sorting key function
  -> Text
  -- ^ Cost-centre name
  -> Text
  -- ^ Module name
  -> CostCentreTree
  -> Maybe (Callee, Seq CallSite)
buildCallSitesOrderBy sortKey name modName tree@CostCentreTree {..} =
  (,) <$> callee <*> callSites
  where
    lookupCallees = Map.lookup (name, modName) costCentreCallSites
    callee = do
      callees <- lookupCallees
      return $ buildCallee name modName callees
    callSites = do
      callees <- lookupCallees
      mapM (buildCallSite tree) $
        Seq.unstableSortBy (flip compare `on` sortKey) callees

buildCallee :: Text -> Text -> Seq CostCentre -> Callee
buildCallee name modName callees = Callee
  { calleeName = name
  , calleeModule = modName
  , calleeEntries = Fold.sum $ costCentreEntries <$> callees
  , calleeTime = Fold.sum $ costCentreIndTime <$> callees
  , calleeAlloc = Fold.sum $ costCentreIndAlloc <$> callees
  , calleeTicks = asum $ costCentreTicks <$> callees
  , calleeBytes = asum $ costCentreBytes <$> callees
  }

buildCallSite :: CostCentreTree -> CostCentre -> Maybe CallSite
buildCallSite CostCentreTree {..} CostCentre {..} = do
  parentNo <- IntMap.lookup costCentreNo costCentreParents
  parent <- IntMap.lookup parentNo costCentreNodes
  return CallSite
    { callSiteCostCentre = parent
    , callSiteContribEntries = costCentreEntries
    , callSiteContribTime = costCentreIndTime
    , callSiteContribAlloc = costCentreIndAlloc
    , callSiteContribTicks = costCentreTicks
    , callSiteContribBytes = costCentreBytes
    }