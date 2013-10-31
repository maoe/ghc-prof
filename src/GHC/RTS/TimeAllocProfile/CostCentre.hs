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
import Data.Text (Text)
import Data.Tree (Tree)
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

profileCallSites :: Text -> Text -> TimeAllocProfile -> Maybe (Tree CallSite)
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
  -> Maybe (Tree CallSite)
profileCallSitesOrderBy sortKey name modName =
  buildCallSitesOrderBy sortKey name modName . profileCostCentreTree

buildCostCentresOrderBy
  :: Ord a
  => (CostCentre -> a)
  -> CostCentreTree
  -> Maybe (Tree CostCentre)
buildCostCentresOrderBy sortKey CostCentreTree {..} = do
  -- Invariant:
  --   The root node (MAIN.MAIN) has the least cost centre ID
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
  -- ^ Cost-cetnre name
  -> Text
  -- ^ Module name
  -> CostCentreTree
  -> Maybe (Tree CallSite)
buildCallSitesOrderBy sortKey name modName CostCentreTree {..} =
  Tree.Node <$> callee <*> callSites
  where
    lookupCallees = Map.lookup (name, modName) costCentreCallSites
    callee = do
      callees <- lookupCallees
      return $ buildCallSite name modName (Fold.toList callees)
    callSites = do
      callees <- lookupCallees
      Tree.unfoldForestM build $
        map (\node -> IntMap.lookup (costCentreNo node) costCentreParents)
          (Fold.toList $ Seq.unstableSortBy (flip compare `on` sortKey) callees)
      where
        build Nothing = do
          rootKey <- listToMaybe $ IntMap.keys costCentreNodes
          root <- IntMap.lookup rootKey costCentreNodes
          return (costCentreToCallSite root, [])
        build (Just callerKey) = do
          caller <- IntMap.lookup callerKey costCentreNodes
          return
            ( costCentreToCallSite caller
            , [IntMap.lookup callerKey costCentreParents]
            )

buildCallSite :: Text -> Text -> [CostCentre] -> CallSite
buildCallSite name modName callees = CallSite
  { callSiteName = name
  , callSiteModule = modName
  , callSiteEntries = sum $ map costCentreEntries callees
  , callSiteIndTime = sum $ map costCentreIndTime callees
  , callSiteIndAlloc = sum $ map costCentreIndAlloc callees
  , callSiteInhTime = sum $ map costCentreInhTime callees
  , callSiteInhAlloc = sum $ map costCentreInhAlloc callees
  , callSiteTicks = asum $ map costCentreTicks callees
  , callSiteBytes = asum $ map costCentreBytes callees
  }

costCentreToCallSite :: CostCentre -> CallSite
costCentreToCallSite CostCentre {..} = CallSite
  { callSiteName = costCentreName
  , callSiteModule = costCentreModule
  , callSiteEntries = costCentreEntries
  , callSiteIndTime = costCentreIndTime
  , callSiteIndAlloc = costCentreIndAlloc
  , callSiteInhTime = costCentreInhTime
  , callSiteInhAlloc = costCentreInhAlloc
  , callSiteTicks = costCentreTicks
  , callSiteBytes = costCentreBytes
  }
