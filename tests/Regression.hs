{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Traversable
import System.Directory
import System.FilePath
import System.IO
import Prelude

import System.IO.Temp
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as TL

import GHC.Prof

#if !MIN_VERSION_directory(1, 2, 3)
import Control.Exception
#endif

main :: IO ()
main = withSystemTempDirectory "test" $ \dir -> withCurrentDirectory dir $
  defaultMain $ testCaseSteps "Regression tests" $ \step -> do
    step "Generating profiling reports"
    profiles <- generateProfiles
    for_ profiles $ \prof -> do
      step $ "Parsing " ++ prof
      assertProfile prof

generateProfiles :: IO [FilePath]
generateProfiles = do
  withFile "hello.hs" WriteMode $ \h ->
    hPutStrLn h $ unlines
      [ "import Control.Exception"
      , "main = evaluate $ fib 100000"
      , "fib n = fibs !! n"
      , "fibs = 0:1:zipWith (+) fibs (tail fibs)"
      ]
  void $ readProcess "ghc" ["-prof", "-rtsopts", "-fforce-recomp", "hello.hs"] ""
  for profilingFlags $ \(name, flag) -> do
    void $ readProcess "./hello" ["+RTS", flag, "-RTS"] ""
    let profName = "hello" <.> name <.> "prof"
    renameFile "hello.prof" profName
    return profName

profilingFlags :: [(String, String)]
profilingFlags =
  [ ("standard", "-p")
  , ("detailed", "-P")
  , ("full", "-pa")
  ]

assertProfile :: FilePath -> Assertion
assertProfile path = do
  text <- TL.readFile path
  case ATL.parse profile text of
    ATL.Done _ prof -> do
      let actual = Set.fromList $ map Similar $ aggregateCostCentres prof
          expected = Set.fromList $ map Similar $ profileTopCostCentres prof
      assertBool
        ("Missing cost centre(s): " ++ show (Set.difference expected actual)) $
          Set.isSubsetOf expected actual
    ATL.Fail _ _ reason -> assertFailure reason

newtype Similar = Similar AggregateCostCentre

instance Show Similar where
  show (Similar a) = show a

instance Eq Similar where
  Similar a == Similar b =
    aggregateCostCentreName a == aggregateCostCentreName b
    && aggregateCostCentreModule a == aggregateCostCentreModule b
    && aggregateCostCentreTime a == aggregateCostCentreTime b
    && aggregateCostCentreAlloc a == aggregateCostCentreAlloc b
    && aggregateCostCentreTicks a == aggregateCostCentreTicks b
    && aggregateCostCentreBytes a == aggregateCostCentreBytes b


instance Ord Similar where
  compare (Similar a) (Similar b) =
    compare (aggregateCostCentreName a) (aggregateCostCentreName b)
    <> compare (aggregateCostCentreModule a) (aggregateCostCentreModule b)
    <> compare (aggregateCostCentreTime a) (aggregateCostCentreTime b)
    <> compare (aggregateCostCentreAlloc a) (aggregateCostCentreAlloc b)
    <> compare (aggregateCostCentreTicks a) (aggregateCostCentreTicks b)
    <> compare (aggregateCostCentreBytes a) (aggregateCostCentreBytes b)

#if !MIN_VERSION_directory(1, 2, 3)
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir io = bracket
  (getCurrentDirectory <* setCurrentDirectory dir)
  (setCurrentDirectory)
  (const io)
#endif
