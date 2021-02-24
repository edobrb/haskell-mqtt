module Bits where

import Data.Bits (shiftL)
import Data.List.Split
import Data.Word (Word8)
import qualified Data.ByteString as BS

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

bitToInteger :: Bool -> Integer
bitToInteger True = 1 :: Integer
bitToInteger False = 0 :: Integer

bitsToInteger :: [Bit] -> Integer
bitsToInteger bits = sum $ map (\(i, v) -> v * ((1 :: Integer) `shiftL` i)) bitsWithIndex
  where
    bitsWithIndex = zip [0 ..] (reverse $ map bitToInteger bits)

bitsToInt :: [Bit] -> Int
bitsToInt bits = sum $ map (\(i, v) -> v * (1 `shiftL` i)) bitsWithIndex
  where
    bitsWithIndex = zip [0 ..] (reverse $ map fromEnum bits)

bitsToWords :: [Bit] -> [Word8]
bitsToWords bits = map bitsToWord8 $ chunksOf 8 bits

bitsToWord8 :: [Bit] -> Word8
bitsToWord8 bits = f $ reverse bits
  where
    f :: [Bit] -> Word8
    f [] = 0
    f (True : cs) = 1 + (f cs * 2)
    f (False : cs) = f cs * 2

fromByteString :: BS.ByteString -> [Bit]
fromByteString bs = Prelude.concatMap (padTo 8 . toBits) (BS.unpack bs)

toByteString :: [Bit] -> BS.ByteString
toByteString bits = BS.pack  $ bitsToWords bits

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
