module Data.Align.Types (
    AlignConfig(..)
  , alignConfig
  , localAlignConfig
  , Step
  , stepLeft, stepRight, stepBoth
  , isMatch, isLeft, isRight
  , Trace(..)
  , tappend
                        ) where


data AlignConfig a s = AlignConfig
  { acPairScore :: a -> a -> s
  , ac_initial_gap_penalty :: s
  , ac_gap_penalty :: s
  }


-- | Configures the scores used when aligning.
-- 
-- The gap scores should be negative in order to be penalties.
alignConfig :: (a -> a -> s)  -- ^ Scoring function
            -> s              -- ^ Initial gap score
            -> s              -- ^ Gap score
            -> AlignConfig a s
alignConfig = AlignConfig

-- | Configuration for local alignment.
localAlignConfig
  :: Num s
  => (a -> a -> s)  -- ^ Scoring function.
  -> s              -- ^ Gap score.
  -> AlignConfig a s
localAlignConfig f = alignConfig f 0

-- | Either an unmatched item or a match.
type Step a = Either (Either a a) (a, a)

stepLeft :: a -> Either (Either a b1) b2
stepLeft = Left . Left
stepRight :: b1 -> Either (Either a b1) b2
stepRight = Left . Right
stepBoth :: a1 -> b -> Either a2 (a1, b)
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

instance Semigroup s => Semigroup (Trace a s) where
  (Trace s1 t1) <> (Trace s2 t2) = Trace (s1 <> s2) (t1 ++ t2)

instance (Show a, Show s) => Show (Trace a s) where
  show (Trace s t) = "Trace(score = " ++ show s ++ ", steps = " ++ show t ++ ")"

tappend :: (Functor f, Num s) =>
           f (Trace a s) -> Trace a s -> f (Trace a s)
mt `tappend` (Trace z (t:_)) =
    fmap (\(Trace s ts) -> Trace (s+z) (t:ts)) mt
