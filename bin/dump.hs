{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ((<$>))
import Data.Tree (drawTree)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Foldable as Fold

import qualified Data.Attoparsec.Text.Lazy as ATL

import GHC.Prof


main :: IO ()
main = do
  file:restArgs <- getArgs
  text <- TLIO.readFile file
  case ATL.parse profile text of
    ATL.Fail unconsumed contexts reason -> fail $ show (unconsumed, contexts, reason)
    ATL.Done _ prof -> case restArgs of
      [] -> Fold.mapM_ putStrLn $ drawTree . fmap makeCCName <$> costCentres prof
      name:modName:_ -> do
        case callSites (T.pack name) (T.pack modName) prof of
          Nothing -> putStrLn "failed to parse call sites"
          Just (callee, callers) -> do
            print callee
            Fold.mapM_ print callers
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
