module CommonPacketFactory where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Word (Word8)
import Packets
import Bits

stringToBits :: String -> [Bit]
stringToBits s = packWithLength d
  where
    encoded = reverse $ concatMap UTF8.encodeChar s
    reducer :: [Bit] -> Word8 -> [Bit]
    reducer bits word = toFixedBits 8 word ++ bits
    d = foldl reducer [] encoded

qosToBits :: QoS -> [Bit]
qosToBits QoS0 = [zero, zero]
qosToBits QoS1 = [zero, one]
qosToBits QoS2 = [one, zero]

packWithLength :: [Bit] -> [Bit]
packWithLength s = msb ++ lsb ++ s
  where
    l = length s `div` 8
    msb = toFixedBits 8 $ l `div` 256
    lsb = toFixedBits 8 $ l `mod` 256

contains :: (Eq a) => Maybe a -> a -> Bool
contains (Just x) y = x == y
contains _ _ = False