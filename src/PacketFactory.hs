module PacketFactory (packetToBits) where

import Bits
import CommonPacketFactory
import Data.Maybe
import Packets
import Utils

packetToBits :: Packet -> [Bit]
packetToBits p@Connect {} = connectPacketHeader ++ remainingLength ++ packet
  where
    remainingLength = toVariableLengthInteger $ length packet `div` 8
    packet = protocolName ++ protocolLevel ++ flags ++ keepAliveBits ++ clientIdBits ++ willTopicBits ++ willPayloadBits ++ usernameBits ++ passwordBits
    protocolName = stringToBits $ name $ protocol p
    protocolLevel = toFixedBits 8 $ level $ protocol p
    usernameFlag = isJust $ credentials p
    passwordFlag = isJust $ credentials p >>= password
    willRetain = fmap retain (willMessage p) `contains` True
    willQoS = qosToBits $ maybe QoS0 qos (willMessage p)
    willFlag = isJust $ willMessage p
    flags = [usernameFlag, passwordFlag, willRetain] ++ willQoS ++ [willFlag, cleanSession p, zero]
    keepAliveBits = toFixedBits 16 $ keepAliveSeconds p
    clientIdBits = stringToBits $ clientId p
    willTopicBits = maybe [] (stringToBits . topic) (willMessage p)
    willPayloadBits = maybe [] (packWithLength . payload) (willMessage p)
    usernameBits = maybe [] (stringToBits . username) (credentials p)
    passwordBits = packWithLength $ fromMaybe [] (password =<< credentials p)
packetToBits p@Connack {} = connackPacketHeader ++ remainingLength ++ packet
  where
    remainingLength = zeros 6 ++ [one, zero]
    packet = sessionPresentBits ++ returnCodeBits
    sessionPresentBits = zeros 7 ++ [sessionPresent p]
    returnCodeBits = toFixedBits 8 $ fromEnum $ returnCode p
packetToBits _ = []
