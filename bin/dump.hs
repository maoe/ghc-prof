{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ((<$>))
import Data.Foldable
import Prelude hiding (concat, foldl)
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Printf

import Data.Scientific
import Data.Tree (drawTree)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO

import GHC.Prof

main :: IO ()
main = do
  (opts, file:restArgs) <- parseOpts =<< getArgs
  text <- TLIO.readFile file
  case decode text of
    Left reason -> fail reason
    Right prof -> case optMode opts of
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
makeCCName cc = printf "%s.%s:%d (%s,%s,%s,%s)"
  (T.unpack $ costCentreModule cc)
  (T.unpack $ costCentreName cc)
  (costCentreNo cc)
  (showScientific $ costCentreInhTime cc)
  (showScientific $ costCentreIndTime cc)
  (showScientific $ costCentreInhAlloc cc)
  (showScientific $ costCentreIndAlloc cc)

makeAggregateCCName :: AggregateCostCentre -> String
makeAggregateCCName aggregate = printf
  "%s%%\t%s%%\t%s.%s"
  (showScientific $ aggregateCostCentreTime aggregate)
  (showScientific $ aggregateCostCentreAlloc aggregate)
  (T.unpack $ aggregateCostCentreModule aggregate)
  (T.unpack $ aggregateCostCentreName aggregate)

showScientific :: Scientific -> String
showScientific = formatScientific Fixed Nothing

data Options = Options
  { optMode :: Mode
  }

defaultOptions :: Options
defaultOptions = Options
  { optMode = TreeMode
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
