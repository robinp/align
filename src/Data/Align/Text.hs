{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module Data.Align.Text (
  -- * Text
  align
  ) where

import qualified Data.List as L (maximumBy)
import Data.Ord (comparing)

-- containers
import qualified Data.Map as M
-- text
import qualified Data.Text as T (Text, length, index)
-- transformers
import Control.Monad.Trans.State.Strict


import Data.Align.Types


-- | 'Data.Align.align', specialized to 'Data.Text.Text'
align :: (Num s, Ord s) =>
         AlignConfig Char s
      -> T.Text -- ^ Left sequence
      -> T.Text -- ^ Right sequence
      -> Trace Char s
align AlignConfig{..} as bs = revTrace $ evalState (go p) M.empty
  where
  p = (lastIndex as, lastIndex bs)
  revTrace (Trace s t) = Trace s (reverse t)
  lastIndex v = T.length v - 1
  --
  go ij = do
    res <- gets (M.lookup ij)
    case res of
        Just r -> return r
        Nothing -> do
            newRes <- pgo ij
            modify (M.insert ij newRes)
            return newRes
  --
  pgo (i, j)
    | i == (-1) || j == (-1) = return $
      if i == j then Trace 0 []
      else if i == (-1)
           then skipInit j stepRight bs
           else skipInit i stepLeft as
    | otherwise = do
      let a = as `T.index` i
          b = bs `T.index` j
      diag  <- go (i-1,j-1) `tappend` Trace (acPairScore a b) [stepBoth a b]
      a_gap <- go (i-1,  j) `tappend` Trace ac_gap_penalty [stepLeft a]
      b_gap <- go (  i,j-1) `tappend` Trace ac_gap_penalty [stepRight b]
      return $ L.maximumBy (comparing traceScore) [diag, a_gap, b_gap]
  --
  skipInit idx stepFun xs =
    let score = ac_initial_gap_penalty * fromIntegral (idx+1)
        tr = reverse [stepFun (xs `T.index` xi) | xi <- [0..idx]]
    in Trace score tr
