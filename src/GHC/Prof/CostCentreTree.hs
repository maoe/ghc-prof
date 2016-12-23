{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
module GHC.Prof.CostCentreTree
  ( aggregateCostCentres
  , aggregateCostCentresOrderBy

  , costCentres
  , costCentresOrderBy

  , aggregateCallSites
  , aggregateCallSitesOrderBy

  , callSites
  , callSitesOrderBy

  , buildAggregateCostCentresOrderBy
  , buildCostCentresOrderBy
  , buildCallSitesOrderBy
  , buildAggregateCallSitesOrderBy
  ) where
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Data.Function (on)
import Data.List
import Data.Maybe (listToMaybe)
import Prelude hiding (mapM)
import qualified Data.Foldable as Fold

import Data.Text (Text)
import Data.Tree (Tree)
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import GHC.Prof.Types as Types

#if MIN_VERSION_containers(0, 5, 0)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
#else
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
#endif

-- | Build a list of cost-centres from a profiling report ordered by the time
-- spent and the amount of allocation.
aggregateCostCentres :: Profile -> [AggregateCostCentre]
aggregateCostCentres = aggregateCostCentresOrderBy sortKey
  where
    sortKey = aggregateCostCentreTime &&& aggregateCostCentreAlloc

-- | Build a list of cost-centres from a profling report ordered by the given
-- key.
aggregateCostCentresOrderBy
  :: Ord a
  => (AggregateCostCentre -> a)
  -- ^ Sorting key function
  -> Profile
  -> [AggregateCostCentre]
aggregateCostCentresOrderBy sortKey =
  buildAggregateCostCentresOrderBy sortKey . profileCostCentreTree

-- | Build a tree of cost-centres from a profiling report.
costCentres :: Profile -> Maybe (Tree CostCentre)
costCentres = costCentresOrderBy sortKey
  where
    sortKey =
      costCentreInhTime &&& costCentreIndTime &&&
      costCentreInhAlloc &&& costCentreIndAlloc

-- | Build a tree of cost-centres from a profiling report.
-- Nodes are sorted by the given key function for each level
-- of the tree.
costCentresOrderBy
  :: Ord a
  => (CostCentre -> a)
  -- ^ Sorting key function
  -> Profile
  -> Maybe (Tree CostCentre)
costCentresOrderBy sortKey =
  buildCostCentresOrderBy sortKey . profileCostCentreTree

aggregateCallSites
  :: Text
  -> Text
  -> Profile
  -> Maybe (AggregateCostCentre, [CallSite AggregateCostCentre])
aggregateCallSites = aggregateCallSitesOrderBy sortKey
  where
    sortKey = callSiteContribTime &&& callSiteContribAlloc
      &&& aggregateCostCentreTime . callSiteCostCentre
      &&& aggregateCostCentreAlloc . callSiteCostCentre

aggregateCallSitesOrderBy
  :: Ord a
  => (CallSite AggregateCostCentre -> a)
  -> Text
  -> Text
  -> Profile
  -> Maybe (AggregateCostCentre, [CallSite AggregateCostCentre])
aggregateCallSitesOrderBy sortKey name modName =
  buildAggregateCallSitesOrderBy sortKey name modName . profileCostCentreTree

-- | Build a list of call-sites (caller functions) for a specified
-- cost-centre name and module name.
callSites
  :: Text
  -- ^ Cost-centre name
  -> Text
  -- ^ Module name
  -> Profile
  -> Maybe (AggregateCostCentre, [CallSite CostCentre])
callSites = callSitesOrderBy sortKey
  where
    sortKey = callSiteContribTime &&& callSiteContribAlloc
      &&& costCentreIndTime . callSiteCostCentre
      &&& costCentreIndAlloc . callSiteCostCentre

-- | Build a list of call-sites (caller function) for a specified
-- cost-centre name and module name. Nodes are sorted by the given
-- key function.
callSitesOrderBy
  :: Ord a
  => (CallSite CostCentre -> a)
  -- ^ Sorting key function
  -> Text
  -- ^ Cost-centre name
  -> Text
  -- ^ Module name
  -> Profile
  -> Maybe (AggregateCostCentre, [CallSite CostCentre])
callSitesOrderBy sortKey name modName =
  buildCallSitesOrderBy sortKey name modName . profileCostCentreTree

-----------------------------------------------------------

buildAggregateCostCentresOrderBy
  :: Ord a
  => (AggregateCostCentre -> a)
  -> CostCentreTree
  -> [AggregateCostCentre]
buildAggregateCostCentresOrderBy sortKey CostCentreTree {..} =
  sortBy (flip compare `on` sortKey) $ Map.elems $ costCentreAggregate

buildCostCentresOrderBy
  :: Ord a
  => (CostCentre -> a)
  -- ^ Sorting key function
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
          !children = maybe [] Fold.toList $ do
            nodes <- IntMap.lookup key costCentreChildren
            return $ costCentreNo
              <$> sortBy (flip compare `on` sortKey) (Set.toList nodes)

buildAggregateCallSitesOrderBy
  :: Ord a
  => (CallSite AggregateCostCentre -> a)
  -- ^ Sorting key function
  -> Text
  -- ^ Cost centre name
  -> Text
  -- ^ Module name
  -> CostCentreTree
  -> Maybe (AggregateCostCentre, [CallSite AggregateCostCentre])
buildAggregateCallSitesOrderBy sortKey name modName tree@CostCentreTree {..} =
  (,) <$> callee <*> callers
  where
    callee = Map.lookup (name, modName) costCentreAggregate
    callers = do
      callees <- Map.lookup (name, modName) costCentreCallSites
      sortBy (flip compare `on` sortKey) . Map.elems
        <$> foldM (buildAggregateCallSite tree) Map.empty (Set.toList callees)

buildAggregateCallSite
  :: CostCentreTree
  -> Map.Map (Text, Text) (CallSite AggregateCostCentre)
  -> CostCentre
  -> Maybe (Map.Map (Text, Text) (CallSite AggregateCostCentre))
buildAggregateCallSite CostCentreTree {..} parents CostCentre {..} = do
  parentNo <- IntMap.lookup costCentreNo costCentreParents
  parent <- IntMap.lookup parentNo costCentreNodes
  let parentName = Types.costCentreName parent
      parentModule = Types.costCentreModule parent
  aggregate <- Map.lookup (parentName, parentModule) costCentreAggregate
  return $! Map.insertWith
    mergeCallSites
    (parentName, parentModule)
    CallSite
      { callSiteCostCentre = aggregate
      , callSiteContribEntries = costCentreEntries
      , callSiteContribTime = costCentreIndTime
      , callSiteContribAlloc = costCentreIndAlloc
      , callSiteContribTicks = costCentreTicks
      , callSiteContribBytes = costCentreBytes
      }
    parents

mergeCallSites :: CallSite a -> CallSite a -> CallSite a
mergeCallSites a b = a
  { callSiteContribEntries = callSiteContribEntries a + callSiteContribEntries b
  , callSiteContribTime = callSiteContribTime a + callSiteContribTime b
  , callSiteContribAlloc = callSiteContribAlloc a + callSiteContribAlloc b
  , callSiteContribTicks = (+)
    <$> callSiteContribTicks a
    <*> callSiteContribTicks b
  , callSiteContribBytes = (+)
    <$> callSiteContribBytes a
    <*> callSiteContribBytes b
  }

buildCallSitesOrderBy
  :: Ord a
  => (CallSite CostCentre -> a)
  -- ^ Sorting key function
  -> Text
  -- ^ Cost-centre name
  -> Text
  -- ^ Module name
  -> CostCentreTree
  -> Maybe (AggregateCostCentre, [CallSite CostCentre])
buildCallSitesOrderBy sortKey name modName tree@CostCentreTree {..} =
  (,) <$> callee <*> callers
  where
    callee = Map.lookup (name, modName) costCentreAggregate
    callers = do
      callees <- Map.lookup (name, modName) costCentreCallSites
      sortBy (flip compare `on` sortKey)
        <$> mapM (buildCallSite tree) (Set.toList callees)

buildCallSite
  :: CostCentreTree
  -> CostCentre
  -> Maybe (CallSite CostCentre)
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
