module Bits where

import Data.Bits (shiftL)

type Bit = Bool


zero :: Bit
zero = False
one :: Bit
one = True

zeros :: Int -> [Bit]
zeros n = replicate n False

ones :: Int -> [Bit]
ones n = replicate n True

newtype Bits = Bits [Bit] deriving (Eq)

instance Show Bits where
  show (Bits bits) = insert 8 '\n' (map (\b -> if b then '1' else '0') bits)
    where
      insert :: Int -> a -> [a] -> [a]
      insert n y = countdown n
        where
          countdown 0 xss = y : countdown n xss -- reset to original n
          countdown _ [] = []
          countdown m (x : xss) = x : countdown (m -1) xss

bitsFrom :: (Integral a) => a -> [Bit]
bitsFrom 0 = [zero]
bitsFrom 1 = [one]
bitsFrom v = bitsFrom (v `div` 2) ++ [odd v]

bitsToInt :: [Bit] -> Int
bitsToInt bits = sum (map (\(i, v) -> v * (1 `shiftL` i)) bitsIndex)
  where
    bitsIndex = zip [0 ..] (reverse (map fromEnum bits))

padTo :: Int -> [Bit] -> [Bit]
padTo n xs
  | d > 0 = replicate d zero ++ xs
  | d < 0 = reverse (take n (reverse xs))
  | otherwise = xs
  where
    d = n - length xs
