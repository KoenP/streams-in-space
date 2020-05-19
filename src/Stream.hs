module Stream where

import Prelude hiding (iterate, zipWith, scanl)
import Data.List (intersperse)
import Control.Applicative
import System.IO.Unsafe (unsafeInterleaveIO)
import Vec
import Linear.Epsilon

data Stream a = (:.) { hd :: a, tl :: Stream a }
infixr :.

-- Select an element from a stream based on an index.
(#) :: Stream a -> Integer -> a
(a :. as) # 0 = a
(_ :. as) # n = as # (n-1)

-- Get the first n elements of a stream.
peek :: Integer -> Stream a -> [a]
peek 0 _         = []
peek n (a :. as) = a : peek (n-1) as

-- traceStream :: (a -> String) -> Stream a -> Stream a
-- traceStream show (a :. as) = trace (show a) a :. traceStream show as

-- Let's use a bogus Show instance to save me some typing.
instance Show a => Show (Stream a) where
  show = concat . (++ ["..."]) . intersperse ", " . map show . peek 20

-- Functor and Applicative instances, these will be our main ways of 
instance Functor Stream where
  fmap f (a :. as) = f a :. fmap f as

instance Applicative Stream where
  pure a                  = a :. pure a
  (f :. fs) <*> (a :. as) = f a :. (fs <*> as)

instance Num a => Num (Stream a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Stream a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational

(*~) :: VectorSpace v a => a -> Stream v -> Stream v
a *~ s = fmap (a *^) s

(~*) :: VectorSpace v a => Stream v -> a -> Stream v
(~*) = flip (*~)

(~/) :: VectorSpace v a => Stream v -> a -> Stream v
v ~/ a = fmap (^/ a) v

iterate :: (a -> a) -> a -> Stream a
iterate f a = a :. iterate f (f a)

scanl :: (b -> a -> b) -> b -> Stream a -> Stream b
scanl f b0 (a :. as) = b0 :. scanl f (f b0 a) as

transfer :: (b -> a -> b) -> b -> Stream a -> Stream b
transfer f b0 (a :. as) = let b = f b0 a
                          in b :. transfer f b as

history :: Stream a -> Stream [a]
history = transfer (\b a -> a:b) []

inputStream :: IO a -> IO (Stream a)
inputStream action = do
  a  <- unsafeInterleaveIO action
  as <- unsafeInterleaveIO (inputStream action)
  return (a :. as)

cycle :: [a] -> Stream a
cycle l = foldr (:.) undefined (Prelude.cycle l)

-- Detects rising edges.
-- Also fires at t=0 if the input stream is True at t=0.
edge :: Stream Bool -> Stream Bool
edge bs = (\b0 b1 -> not b0 && b1) <$> (False :. bs) <*> bs
