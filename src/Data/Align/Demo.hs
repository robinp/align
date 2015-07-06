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

testIn1 = "dopple"
testIn2 = "applied"

alignedGlobal =
  align sampleGlobalConfig (V.fromList testIn1) (V.fromList testIn2)

debug = putStrLn . debugAlign . trace

printAlignedGlobal = debug alignedGlobal

-- * Multi-sequence fun.

-- | Example from https://www.biostat.wisc.edu/bmi576/lectures/multiple-alignment.pdf
nucs =
  [ "ATTGCCATT"
  , "ATGGCCATT"
  , "ATCCAATTTT"
  , "ATCTTCTT"
  , "ATTGCCGATT"
  ]

alignNuc a b = align sampleGlobalConfig (V.fromList a) (V.fromList b)

alignedNucPairs = do
  n <- tail nucs
  return $ alignNuc (head nucs) n

printAlignedNucPairs = mapM_ (\x -> debug x >> putStrLn "") alignedNucPairs

alignedNucStar =
  centerStar sampleGlobalConfig $ zip [1..] (map V.fromList nucs)

printAlignedNucStar = putStrLn . debugMultiAlign . multiTrace $ alignedNucStar
