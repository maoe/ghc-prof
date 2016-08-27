{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ((<$>))
import Data.Foldable
import Prelude hiding (concat, foldl)
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Printf

import Data.Tree (drawTree)
import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO

import GHC.Prof

main :: IO ()
main = do
  (opts, file:restArgs) <- parseOpts =<< getArgs
  text <- TLIO.readFile file
  case ATL.parse profile text of
    ATL.Fail unconsumed contexts reason ->
      fail $ show (unconsumed, contexts, reason)
    ATL.Done _ prof -> case optMode opts of
      AggregateMode ->
        traverse_ (putStrLn . makeAggregateCCName) $ aggregateCostCentres prof
      TreeMode -> case restArgs of
        [] ->
          traverse_ putStrLn $ drawTree . fmap makeCCName <$> costCentres prof
        name:modName:_ -> do
          case callSites (T.pack name) (T.pack modName) prof of
            Nothing -> putStrLn "failed to parse call sites"
            Just (callee, callers) -> do
              print callee
              traverse_ print callers
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

makeAggregateCCName :: AggregateCostCentre -> String
makeAggregateCCName aggregate = printf
  "time %f%%\talloc %f%%\t%s.%s"
  (aggregateCostCentreTime aggregate)
  (aggregateCostCentreAlloc aggregate)
  (T.unpack $ aggregateCostCentreModule aggregate)
  (T.unpack $ aggregateCostCentreName aggregate)

data Options = Options
  { optMode :: Mode
  }

defaultOptions :: Options
defaultOptions = Options
  { optMode = AggregateMode
  }

data Mode = AggregateMode | TreeMode

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['a'] ["aggregate"]
    (NoArg (\opts -> opts { optMode = AggregateMode }))
      "Aggregate mode"
  ]

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (opts, rest, []) -> return (foldl (flip id) defaultOptions opts, rest)
  (_, _, errs) -> fail $ concat errs ++ usageInfo header options
  where
    header = "Usage: dump [OPTION...] ..."
