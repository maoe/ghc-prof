{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ((<$>))
import Data.Tree (drawTree)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Foldable as Fold

import qualified Data.Attoparsec.Text.Lazy as ATL

import GHC.RTS.TimeAllocProfile


main :: IO ()
main = do
  file:restArgs <- getArgs
  text <- TLIO.readFile file
  let ATL.Done _ prof = ATL.parse timeAllocProfile text
  case restArgs of
    [] -> Fold.mapM_ putStrLn $ drawTree . fmap makeCCName <$> profileCostCentres prof
    name:modName:_ -> do
      case profileCallSites (T.pack name) (T.pack modName) prof of
        Nothing -> putStrLn "failed to parse call sites"
        Just (callee, callSites) -> do
          print callee
          Fold.mapM_ print callSites
    _ -> fail "Invalid parameters"

makeCCName :: CostCentre -> String
makeCCName cc = T.unpack (costCentreModule cc)
  ++ "."
  ++ T.unpack (costCentreName cc)
  ++ ":"
  ++ show (costCentreNo cc)
  ++ " ("
  ++ show (costCentreInhTime cc)
  ++ ","
  ++ show (costCentreIndTime cc)
  ++ ","
  ++ show (costCentreInhAlloc cc)
  ++ ","
  ++ show (costCentreIndAlloc cc)
  ++ ")"

