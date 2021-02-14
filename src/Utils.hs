module Utils where

import Bits
import Data.List.Split

toVariableLengthInteger :: Int -> [Bit]
toVariableLengthInteger n
  | d == 0 = padTo 8 (toBits r)
  | otherwise = (one : tail (toVariableLengthInteger r)) ++ toVariableLengthInteger d
  where
    d = n `div` 128
    r = n `mod` 128

fromVariableLengthInteger :: [Bit] -> Maybe Int
fromVariableLengthInteger bits
  | lb > 4 || lb <= 0 || l `mod` 8 /= 0 = Nothing
  | head (last bytes) == one || any (\b -> head b == zero) (init bytes) = Nothing
  | otherwise = Just (bitsToInt valueBits)
  where
    lb = length bytes
    l = length bits
    bytes = chunksOf 8 bits
    valueBits = concatMap tail (reverse bytes)
