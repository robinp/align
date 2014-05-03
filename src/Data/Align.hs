{-# LANGUAGE RecordWildCards #-}
module Data.Align
  ( align
  , AlignConfig
  , alignConfig
  , Step
  , Trace, traceScore, trace
  , windowedAlign
  , debugAlign
  ) where

import Data.Function (fix)
import qualified Data.List as L
import Data.MemoUgly
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Debug.Trace as D

data AlignConfig a s = AlignConfig
  { acPairScore :: a -> a -> s
  , ac_initial_gap_penalty :: s
  , ac_gap_penalty :: s
  }

alignConfig :: (a -> a -> s)  -- ^ Scoring function.
            -> s              -- ^ Initial gap penalty.
            -> s              -- ^ Gap penalty.
            -> AlignConfig a s
alignConfig = AlignConfig

-- | Either an unmatched item or a match.
type Step a = Either (Either a a) (a, a)

stepLeft = Left . Left
stepRight = Left . Right
stepBoth a b = Right (a,b)

isMatch :: Step a -> Bool
isMatch (Right _) = True
isMatch _ = False

isLeft :: Step a -> Bool
isLeft (Left (Left _)) = True
isLeft _ = False

isRight :: Step a -> Bool
isRight (Left (Right _)) = True
isRight _ = False

-- | The result of the alignment.
data Trace a s = Trace
  { traceScore :: s
  , trace :: [Step a]
  }

instance (Show a, Show s) => Show (Trace a s) where
  show (Trace s t) = "Trace(score = " ++ show s ++ ", steps = " ++ show t ++ ")"

(Trace s ts) `tappend` (Trace z (t:_)) = Trace (s+z) (t:ts)

-- | Utility for displaying a Char-based alignment.
debugAlign :: [Step Char] -> String
debugAlign = go [] []
  where
  go as bs [] = reverse as ++ "\n" ++ reverse bs
  go as bs (t:ts) = case t of
    Left (Left c)  -> go (c:as) ('-':bs) ts
    Left (Right c) -> go ('-':as) (c:bs) ts
    Right (c, d)   -> go (c:as) (d:bs) ts

-- | Aligns two sequences.
--
-- >>> :{
-- let tr = align
--            (alignConfig (\a b -> if a == b then 1 else (-0.25 :: Double)) 
--                         (-0.5) (-1))
--            (Data.Vector.fromList "dopple")
--            (Data.Vector.fromList "applied")
-- in do
--    print $ traceScore tr
--    putStrLn . debugAlign . trace $ tr
-- :}
-- 1.25
-- doppl-e-
-- -applied
align :: (G.Vector v a, Num s, Eq s, Ord s)
  => AlignConfig a s
  -> v a  -- ^ Left sequence.
  -> v a  -- ^ Right sequence.
  -> Trace a s
align (AlignConfig {..}) as bs =
  revTrace . fix (memo . go) $ (lastIndex as, lastIndex bs)
  where
  revTrace (Trace s t) = Trace s (reverse t)
  lastIndex v = G.length v - 1
  --
  go k (i,j)
    | i == (-1) || j == (-1) = 
      if i == j then Trace 0 []
      else if i == (-1)
           then skipInit j stepRight bs
           else skipInit i stepLeft as
    | otherwise =
      let a = as G.! i
          b = bs G.! j
          diag  = k (i-1,j-1) `tappend` Trace (acPairScore a b) [stepBoth a b]
          a_gap = k (i-1,  j) `tappend` Trace ac_gap_penalty [stepLeft a]
          b_gap = k (  i,j-1) `tappend` Trace ac_gap_penalty [stepRight b]
      in L.maximumBy (comparing traceScore) [diag, a_gap, b_gap]
  --
  skipInit idx stepFun xs =
    let score = ac_initial_gap_penalty * fromIntegral (idx+1)
        tr = reverse [stepFun (xs G.! xi) | xi <- [0..idx]]
    in Trace score tr

-- | Aligns long streams by performing alignment on windowed sections.
windowedAlign :: (Num s, Eq s, Ord s)
  => AlignConfig a s 
  -> Int  -- ^ Window size.
  -> [a]  -- ^ Left stream.
  -> [a]  -- ^ Right stream.
  -> [Step a]  -- ^ Alignment result.
windowedAlign cfg w as bs = go as bs []
  where
  go as [] rs = concat . reverse $ (map stepLeft as):rs
  go [] bs rs = concat . reverse $ (map stepRight bs):rs
  go as bs rs =
    let ahs = take w as
        bhs = take w bs
        tr0 = trace $ align cfg (V.fromList ahs) (V.fromList bhs)
        tr = let matched = dropTrailingMismatches tr0
                 len = length matched
             in if len <= 1
                then matched
                -- Give opportunity for recovery given new inputs in next go.
                -- TODO this needs some experimenting with (or even option)
                else take (len `div` 2) matched
    in if null tr
       then let (a:ax) = as
                (b:bx) = bs
            -- Degenerate case, mimic some progress to avoid getting stuck.
            in go ax bx ([stepLeft a, stepRight b]:rs)
       else let ac = countMatchOr isLeft tr
                bc = countMatchOr isRight tr
            in go (drop ac as) (drop bc bs) (tr:rs)
  --
  dropTrailingMismatches =
    reverse . dropWhile (not . isMatch) . reverse
  --
  countMatchOr f = length . filter (\s -> isMatch s || f s)
