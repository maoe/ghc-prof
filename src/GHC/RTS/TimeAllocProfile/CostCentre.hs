{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-} -- for debugging
module GHC.RTS.TimeAllocProfile.CostCentre
  ( profileCostCentres
  , profileCallSites

  , buildCostCentres
  , buildCallSites
  ) where
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (asum)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Tree (Tree)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree

import GHC.RTS.TimeAllocProfile.Types

profileCostCentres :: TimeAllocProfile -> Maybe (Tree CostCentre)
profileCostCentres = buildCostCentres . profileCostCentreTree

profileCallSites :: Text -> Text -> TimeAllocProfile -> Maybe (Tree CallSite)
profileCallSites name modName =
  buildCallSites name modName . profileCostCentreTree

buildCostCentres :: CostCentreTree -> Maybe (Tree CostCentre)
buildCostCentres CostCentreTree {..} = do
  -- Invariant:
  --   The root node (MAIN.MAIN) has the least cost centre ID
  rootKey <- listToMaybe $ IntMap.keys costCentreNodes
  Tree.unfoldTreeM build rootKey
  where
    build key = do
      node <- IntMap.lookup key costCentreNodes
      let children = maybe [] reverse $ IntMap.lookup key costCentreChildren
      return (node, children)

buildCallSites :: Text -> Text -> CostCentreTree -> Maybe (Tree CallSite)
buildCallSites name modName CostCentreTree {..} =
  Tree.Node <$> callee <*> callSites
  where
    calleeKeys = Map.lookup (name, modName) costCentreCallSites
    callee = do
      keys <- calleeKeys
      callees <- forM keys $ \key -> IntMap.lookup key costCentreNodes
      return $ buildCallSite name modName callees
    callSites = do
      keys <- calleeKeys
      Tree.unfoldForestM build $
        map (\key -> IntMap.lookup key costCentreParents) keys
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
