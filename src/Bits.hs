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

toBits :: (Integral a) => a -> [Bit]
toBits 0 = [zero]
toBits 1 = [one]
toBits v = toBits (v `div` 2) ++ [odd v]

toFixedBits :: (Integral a) => Int -> a -> [Bit]
toFixedBits n x = padTo n $ toBits x

bitsToInteger :: [Bit] -> Integer
bitsToInteger bits = sum $ map (\(i, v) -> v * ((1 :: Integer) `shiftL` i)) bitsWithIndex
  where
    bitsWithIndex = zip [0 ..] (reverse $ map boolToInteger bits)
    boolToInteger True = 1 :: Integer
    boolToInteger False = 0 :: Integer

bitsToInt :: [Bit] -> Int
bitsToInt bits = sum $ map (\(i, v) -> v * (1 `shiftL` i)) bitsWithIndex
  where
    bitsWithIndex = zip [0 ..] (reverse $ map fromEnum bits)

padTo :: Int -> [Bit] -> [Bit]
padTo n xs
  | d > 0 = replicate d zero ++ xs
  | d < 0 = reverse (take n (reverse xs))
  | otherwise = xs
  where
    d = n - length xs

toWord8 :: [Bit] -> [Bit]
toWord8 = padTo 8

toWord16 :: [Bit] -> [Bit]
toWord16 = padTo 16