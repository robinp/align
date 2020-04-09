-- | Browse the source of this module to see usage examples.
module Data.Align.Demo where

import Data.Align
import qualified Data.Vector as V

-- * Global alignment.

sampleGlobalConfig :: (Eq a) => AlignConfig a Double
sampleGlobalConfig = alignConfig
  (\a b -> if a == b then 1 else -0.25)
  (-0.5)
  (-1)

testIn1, testIn2 :: String
testIn1 = "dopple"
testIn2 = "applied"

alignedGlobal :: Trace Char Double
alignedGlobal =
  align sampleGlobalConfig (V.fromList testIn1) (V.fromList testIn2)

debug :: Trace Char s -> IO ()
debug = putStrLn . debugAlign . trace

printAlignedGlobal :: IO ()
printAlignedGlobal = debug alignedGlobal

-- * Multi-sequence fun.

-- | Example from https://www.biostat.wisc.edu/bmi576/lectures/multiple-alignment.pdf
nucs :: [String]
nucs =
  [ "ATTGCCATT"
  , "ATGGCCATT"
  , "ATCCAATTTT"
  , "ATCTTCTT"
  , "ATTGCCGATT"
  ]

alignNuc :: Eq a => [a] -> [a] -> Trace a Double
alignNuc a b = align sampleGlobalConfig (V.fromList a) (V.fromList b)

alignedNucPairs :: [Trace Char Double]
alignedNucPairs = do
  n <- tail nucs
  return $ alignNuc (head nucs) n

printAlignedNucPairs :: IO ()
printAlignedNucPairs = mapM_ (\x -> debug x >> putStrLn "") alignedNucPairs

alignedNucStar :: MultiTrace Integer Char Double
alignedNucStar =
  centerStar sampleGlobalConfig $ zip [1..] (map V.fromList nucs)

printAlignedNucStar :: IO ()
printAlignedNucStar = putStrLn . debugMultiAlign . multiTrace $ alignedNucStar
