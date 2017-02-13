{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
module GHC.Prof.CostCentreTree
  ( -- * Cost center breakdown
  -- ** Aggregate cost centres
    aggregatedCostCentres
  , aggregatedCostCentresOrderBy

  -- ** Cost centre trees
  , costCentres
  , costCentresOrderBy

  -- * Call site breakdown
  -- ** Aggregate call sites
  , aggregateCallSites
  , aggregateCallSitesOrderBy

  -- ** Call sites
  , callSites
  , callSitesOrderBy

  -- * Module breakdown
  , aggregateModules
  , aggregateModulesOrderBy

  -- * Low level functions
  , buildAggregatedCostCentresOrderBy
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

import Control.Monad.Extras (seqM)
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
aggregatedCostCentres :: Profile -> [AggregatedCostCentre]
aggregatedCostCentres = aggregatedCostCentresOrderBy sortKey
  where
    sortKey = aggregatedCostCentreTime &&& aggregatedCostCentreAlloc

-- | Build a list of cost-centres from a profling report ordered by the given
-- key.
aggregatedCostCentresOrderBy
  :: Ord a
  => (AggregatedCostCentre -> a)
  -- ^ Sorting key function
  -> Profile
  -> [AggregatedCostCentre]
aggregatedCostCentresOrderBy sortKey =
  buildAggregatedCostCentresOrderBy sortKey . profileCostCentreTree

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

-- | Build a list of call sites (caller functions of a cost centre) aggregated
-- by their cost centre names and module names.
aggregateCallSites
  :: Text
  -- ^ Cost centre name
  -> Text
  -- ^ Module name
  -> Profile
  -> Maybe (AggregatedCostCentre, [CallSite AggregatedCostCentre])
aggregateCallSites = aggregateCallSitesOrderBy sortKey
  where
    sortKey = callSiteContribTime &&& callSiteContribAlloc
      &&& aggregatedCostCentreTime . callSiteCostCentre
      &&& aggregatedCostCentreAlloc . callSiteCostCentre

-- | Build a list of call sites (caller functions of a cost centre) aggregated
-- by their cost centre names and module names. Call sites are sorted by the
-- given key function.
aggregateCallSitesOrderBy
  :: Ord a
  => (CallSite AggregatedCostCentre -> a)
  -- ^ Sorting key function
  -> Text
  -- ^ Cost centre name
  -> Text
  -- ^ Module name
  -> Profile
  -> Maybe (AggregatedCostCentre, [CallSite AggregatedCostCentre])
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
  -> Maybe (AggregatedCostCentre, [CallSite CostCentre])
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
  -> Maybe (AggregatedCostCentre, [CallSite CostCentre])
callSitesOrderBy sortKey name modName =
  buildCallSitesOrderBy sortKey name modName . profileCostCentreTree

-- | Break down aggregate cost centres by module sorted by total time and
-- allocation.
aggregateModules
  :: Profile
  -> [AggregateModule]
aggregateModules = aggregateModulesOrderBy sortKey
  where
    sortKey = aggregateModuleTime &&& aggregateModuleAlloc

-- | Break odwn aggregate cost centres by module.
aggregateModulesOrderBy
  :: Ord a
  => (AggregateModule -> a) -- ^ Sorting key function
  -> Profile
  -> [AggregateModule]
aggregateModulesOrderBy sortKey =
    buildAggregateModulesOrderBy sortKey . profileCostCentreTree

-----------------------------------------------------------

buildAggregatedCostCentresOrderBy
  :: Ord a
  => (AggregatedCostCentre -> a)
  -> CostCentreTree
  -> [AggregatedCostCentre]
buildAggregatedCostCentresOrderBy sortKey CostCentreTree {..} =
  sortBy (flip compare `on` sortKey) $
    concatMap Map.elems $ Map.elems $ costCentreAggregate

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
  => (CallSite AggregatedCostCentre -> a)
  -- ^ Sorting key function
  -> Text
  -- ^ Cost centre name
  -> Text
  -- ^ Module name
  -> CostCentreTree
  -> Maybe (AggregatedCostCentre, [CallSite AggregatedCostCentre])
buildAggregateCallSitesOrderBy sortKey name modName tree@CostCentreTree {..} =
  (,) <$> callee <*> callers
  where
    callee = lookupAggregate name modName costCentreAggregate
    callers = do
      callees <- Map.lookup (name, modName) costCentreCallSites
      sortBy (flip compare `on` sortKey) . Map.elems
        <$> foldM (buildAggregateCallSite tree) Map.empty (Set.toList callees)

buildAggregateCallSite
  :: CostCentreTree
  -> Map.Map (Text, Text) (CallSite AggregatedCostCentre)
  -> CostCentre
  -> Maybe (Map.Map (Text, Text) (CallSite AggregatedCostCentre))
buildAggregateCallSite CostCentreTree {..} parents CostCentre {..} = do
  parentNo <- IntMap.lookup costCentreNo costCentreParents
  parent <- IntMap.lookup parentNo costCentreNodes
  let parentName = Types.costCentreName parent
      parentModule = Types.costCentreModule parent
  aggregate <- lookupAggregate parentName parentModule costCentreAggregate
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
  , callSiteContribTicks = seqM $ (+)
    <$> callSiteContribTicks a
    <*> callSiteContribTicks b
  , callSiteContribBytes = seqM $ (+)
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
  -> Maybe (AggregatedCostCentre, [CallSite CostCentre])
buildCallSitesOrderBy sortKey name modName tree@CostCentreTree {..} =
  (,) <$> callee <*> callers
  where
    callee = lookupAggregate name modName costCentreAggregate
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

buildAggregateModulesOrderBy
  :: Ord a
  => (AggregateModule -> a)
  -- ^ Sorting key function
  -> CostCentreTree
  -> [AggregateModule]
buildAggregateModulesOrderBy sortKey CostCentreTree {..} =
  sortBy (flip compare `on` sortKey) $
    Map.foldrWithKey
      (\modName ccs as -> aggregateModule modName ccs : as)
      []
      costCentreAggregate
  where
    aggregateModule modName = Map.foldl' add (emptyAggregateModule modName)
    add aggMod AggregatedCostCentre {..} = aggMod
      { aggregateModuleEntries = seqM $ (+)
        <$> aggregateModuleEntries aggMod
        <*> aggregatedCostCentreEntries
      , aggregateModuleTime =
        aggregateModuleTime aggMod + aggregatedCostCentreTime
      , aggregateModuleAlloc =
        aggregateModuleAlloc aggMod + aggregatedCostCentreAlloc
      , aggregateModuleTicks = seqM $ (+)
        <$> aggregateModuleTicks aggMod
        <*> aggregatedCostCentreTicks
      , aggregateModuleBytes = seqM $ (+)
        <$> aggregateModuleBytes aggMod
        <*> aggregatedCostCentreBytes
      }

lookupAggregate
  :: Text -- ^ Cost centre name
  -> Text -- ^ Module name
  -> Map.Map Text (Map.Map Text AggregatedCostCentre)
  -> Maybe AggregatedCostCentre
lookupAggregate name modName m = Map.lookup modName m >>= Map.lookup name
