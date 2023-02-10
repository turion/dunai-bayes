{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow
import Data.Functor (($>))
import Data.Functor.Compose
import Data.Functor.Identity (Identity (Identity))
import Data.Proxy
import Data.Tuple (swap)
import GHC.TypeNats

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Weighted

-- dunai
import Control.Monad.Trans.MSF (performOnFirstSample)
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (..))

bayesFilter' ::
  (MonadMeasure m, SoftEq sensor) =>
  -- | model
  MSF m input (sensor, state) ->
  -- | external sensor, data source
  MSF m input sensor ->
  MSF m input (sensor, state)
bayesFilter' model sensor = proc input -> do
  output <- sensor -< input
  estimatedState <- bayesFilter model -< (input, output)
  returnA -< (output, estimatedState)

{- | Condition on one output of a distribution.

   p(x,y | theta) ~> p(x | y, theta)
-}
bayesFilter ::
  (MonadMeasure m, SoftEq sensor) =>
  MSF m input (sensor, latent) ->
  -- | external sensor, data source
  MSF m (input, sensor) latent
bayesFilter model = proc (input, measuredOutput) -> do
  (estimatedOutput, estimatedState) <- model -< input
  arrM score -< similarity estimatedOutput measuredOutput
  returnA -< estimatedState

class SoftEq a where
  -- | `similarity a1 a2 == 1` if they are exactly equal, and 0 if they are completely different.
  similarity :: a -> a -> Log Double

  -- | Scores the similarity of the two inputs
  (=~) :: MonadMeasure m => a -> a -> m ()
  a1 =~ a2 = score $ similarity a1 a2

-- FIXME Do I want this?
instance SoftEq Double where
  similarity a1 a2 = normalPdf a1 1 a2

-- | Hard equality check
newtype Exact a = Exact {getExact :: a}
  deriving (Eq, Show, Read, Ord, Enum, Functor, Num, Integral, Real, Fractional, Floating)

instance Eq a => SoftEq (Exact a) where
  similarity a1 a2 = if a1 == a2 then 1 else 0

-- FIXME naming
newtype DigitsPrecision (n :: Nat) a = DigitsPrecision {getDigitsPrecision :: a}
  deriving (Eq, Show, Read, Ord, Enum, Functor, Num, Integral, Real, Fractional, Floating)

digitsPrecisionProxy :: DigitsPrecision n a -> Proxy n
digitsPrecisionProxy _ = Proxy

instance KnownNat n => SoftEq (DigitsPrecision n Double) where
  similarity a1 a2 = normalPdf (getDigitsPrecision a1) (0.1 ^ natVal (digitsPrecisionProxy a1)) (getDigitsPrecision a2)

instance (SoftEq a, SoftEq b) => SoftEq (a, b) where
  similarity (a1, b1) (a2, b2) = similarity a1 a2 + similarity b1 b2 -- FIXME + not *?

-- TODO particle path finder:

-- * Send particles in all directions

-- * Move in the average direction where particles came closest

-- This should work for general control if the control space is not too high dimensional (bang bang to reduce to graph problem)

-- * Is there a MCMC control algorithm?

runPopulationS ::
  forall m a b.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  MSF (Population m) a b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m a [(b, Log Double)]
runPopulationS nParticles resampler msf = runPopulationCl' $ spawn nParticles $> msf
 where
  runPopulationCl' :: Monad m => Population m (MSF (Population m) a b) -> MSF m a [(b, Log Double)]
  runPopulationCl' msfs = MSF $ \a -> do
    -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
    -- FIXME I could also fmap the bs out, then I still have Population m b! (And this can be done in general for any monad...)
    -- FIXME I could also fmap the continuations out, then I save myself a fromWeightedList and a return...
    bAndMSFs <- runPopulation $ flip unMSF a =<< msfs
    let (currentPopulation, continuations) = unzip $ (swap . fmap fst &&& swap . fmap snd) . swap <$> bAndMSFs
        normalizedContinuations =
          runPopulationCl' $
            -- FIXME This normalizes, which introduces bias, whatever that means
            normalize $
              resampler $
                fromWeightedList $
                  return continuations
    return (currentPopulation, normalizedContinuations)

-- FIXME see PR re-adding this to monad-bayes
normalize :: Monad m => Population m a -> Population m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulation

constantParameter ::
  Monad m =>
  m a ->
  MSF m arbitrary a
constantParameter action = performOnFirstSample $ pure <$> action

{-
handle' :: (Monad m, Monad (t m), Functor t') => (forall x . t m x -> m (t' x)) -> t m (MSF (t m) a b) -> MSF m a (t' b)
handle' handler = handleCompose . fmap (morphS (Compose . handler)) . Compose . handler
-}

handle :: (Functor m, Monad (t m)) => (forall x. t m x -> m (t' x)) -> t m (MSF (t m) a b) -> MSF m a (t' b)
handle handler msf = MSF $ \a -> do
  let tb = fst <$> (flip unMSF a =<< msf)
      msf' = snd <$> (flip unMSF a =<< msf)
  b <- handler tb
  return (b, handle handler msf')

{-
handle'' :: (Monad m, Monad (t m)) => (forall x . t m x -> m (t' x)) -> t m (MSF (t m) a b) -> MSF m a (t' b)
handle'' handler = _ . joinS
-}

joinOutput :: Monad m => MSF m a (m b) -> MSF m a b
joinOutput msf = msf >>> arrM id

joinS :: Monad m => m (MSF m a b) -> MSF m a (m b)
joinS msf = MSF $ \a -> do
  let joined = flip unMSF a =<< msf
      tb = fst <$> joined
      msf' = snd <$> joined
  return (tb, joinS msf')

handleCompose :: (Monad m, Monad (Compose m f), Functor f) => Compose m f (MSF (Compose m f) a b) -> MSF m a (f b)
handleCompose msf = MSF $ \a -> do
  bAndMSF <- getCompose $ flip unMSF a =<< msf
  return (fst <$> bAndMSF, handleCompose $ Compose $ return $ snd <$> bAndMSF)

newtype FlipCompose f m a = FlipCompose {getFlipCompose :: Compose m f a}
  deriving (Functor, Applicative)

-- Could prove Monad (FlipCompose f m)
handleCompose'''' :: (Functor f, Monad (Compose m f), Functor m, Monad (FlipCompose f m)) => Compose m f (MSF (Compose m f) a b) -> MSF m a (f b)
handleCompose'''' = handle (getCompose . getFlipCompose) . fmap (morphS FlipCompose) . FlipCompose

handleCompose' :: (Monad m, Monad (Compose m f), Functor f) => MSF (Compose m f) a b -> MSF m a (f b)
handleCompose' = handleCompose . return

handleCompose'' :: (Monad m, Monad (Compose m f), Functor f) => MSF (Compose m f) a b -> MSF m a (f b)
handleCompose'' msf = MSF $ \a -> do
  bAndMSF <- getCompose $ flip unMSF a msf
  return (fst <$> bAndMSF, handleCompose $ Compose $ return $ snd <$> bAndMSF)

{-
handleCompose''' :: (Monad m, Monad (Compose m f), Functor f) => MSF (Compose m f) a b -> MSF m a (f b)
handleCompose''' = _ . (arr getCompose <<<) . snapshot
-}

-- FIXME Is this handleCompose for Compose m Identity or Compose Identity m?

-- | Keep running the 'MSF', but return the output in the current context.
snapshot :: Functor m => MSF m a b -> MSF m a (m b)
snapshot msf = MSF $ \a -> do
  let b = fst <$> unMSF msf a
      msf' = snd <$> unMSF msf a
  (b,) . snapshot <$> msf'

{-
snapshot' :: (Functor m, Monad m, Monad (Compose Identity m)) => MSF m a b -> MSF m a (m b)
snapshot' = _ . handleCompose' . morphS (Compose . Identity)
-}

snapshot'' :: (Functor m, Monad m, Monad (Compose m m)) => MSF m a b -> MSF m a (m b)
snapshot'' = handleCompose' . morphS (Compose . fmap return)

accumulate' :: (Functor m, Monad m, Monad (Compose Identity m)) => MSF m a b -> MSF Identity a (m b)
accumulate' = handleCompose' . morphS (Compose . Identity)

collapseS :: MonadMeasure m => MSF (Population m) a b -> MSF m a b
collapseS = morphS collapse

-- FIXME unit test. Does this what I think it does?
properS :: MonadDistribution m => MSF (Population m) a b -> MSF (Weighted m) a b
properS = morphS proper

-- FIXME separate module. actually, separate package
-- And search whether such a package already exists first.
class Statistical a where
  statistic :: [(a, Double)] -> a

newtype Average a = Average {getAverage :: a}
  deriving (Num, Fractional)

instance Fractional a => Statistical (Average a) where
  -- FIXME realToFrac isn't nice, but unfortunately we're stuck with Doubles
  statistic = sum . fmap (uncurry (*) . second realToFrac)

-- FIXME try coerce
average :: Fractional a => [(a, Double)] -> a
average = getAverage . statistic . fmap (first Average)
