module Packets where

import Bits
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Maybe
import Data.Word (Word8)
import Utils

data ConnectReturnCode
  = ConnectionAccepted
  | UnacceptableProtocolVersion
  | IdentifierRejected
  | ServerUnavailable
  | BadUsernameOrPassword
  | NotAuthorized
  deriving (Enum, Eq, Show)

data QoS = QoS0 | QoS1 | QoS2 deriving (Enum, Eq, Show)

data Protocol = Protocol {name :: String, level :: Int} deriving (Eq, Show)

data Credential = Credential {username :: String, password :: Maybe String} deriving (Eq, Show)

data ApplicationMessage = ApplicationMessage {retain :: Bool, qos :: QoS, topic :: String, payload :: [Bit]} deriving (Eq, Show)

data Packet
  = Connect
      { protocol :: Protocol,
        cleanSession :: Bool,
        keepAliveSeconds :: Int,
        clientId :: String,
        credentials :: Maybe Credential,
        willMessage :: Maybe ApplicationMessage
      }
  | Connack {sessionPresent :: Bool, returnCode :: ConnectReturnCode}
  | Puback {packedID :: Int}
  | Pubrec {packedID :: Int}
  deriving (Eq, Show)

controlPacketType :: Int -> [Bit]
controlPacketType = toFixedBits 4

packetIdentifier :: Int -> [Bit]
packetIdentifier = toFixedBits 16

stringToBits :: String -> [Bit]
stringToBits s = msb ++ lsb ++ d
  where
    encoded = reverse $ concatMap UTF8.encodeChar s
    encodedLength = length encoded
    msb = toFixedBits 8 $ encodedLength `div` 256
    lsb = toFixedBits 8 $ encodedLength `mod` 256
    reducer :: [Bit] -> Word8 -> [Bit]
    reducer bits word = toFixedBits 8 word ++ bits
    d = foldl reducer [] encoded

qosToBits :: QoS -> [Bit]
qosToBits QoS0 = [zero, zero]
qosToBits QoS1 = [zero, one]
qosToBits QoS2 = [one, zero]

contains :: (Eq a) => Maybe a -> a -> Bool
contains (Just x) y = x == y
contains _ _ = False

packetToBits :: Packet -> [Bit]
packetToBits p@Connect {} = controlPacketType 1 ++ zeros 4 ++ remainingLength ++ packet
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
    willPayloadBits = maybe [] payload (willMessage p)
    usernameBits = maybe [] (stringToBits . username) (credentials p)
    passwordBits = maybe [] stringToBits (credentials p >>= password)
packetToBits p@Connack {} = controlPacketType 2 ++ zeros 4 ++ remainingLength ++ packet
  where
    remainingLength = zeros 6 ++ [one, zero]
    packet = sessionPresentBits ++ returnCodeBits
    sessionPresentBits = zeros 7 ++ [sessionPresent p]
    returnCodeBits = toFixedBits 8 $ fromEnum $ returnCode p
packetToBits _ = []

bitsToPacket :: [Bit] -> (Maybe Packet, [Bit])
bitsToPacket = failure

failure :: [a] -> (Maybe b, [a])
failure as = (Nothing, as)

minimumLength :: Int -> [a] -> (Maybe (), [a])
minimumLength n as
  | length as >= n = (Just (), as)
  | otherwise = failure as



controlPacketTypeParser :: [Bit] -> (Maybe Int, [Bit])
controlPacketTypeParser bits
  | l >= 8 = (Just $ bitsToInt $ take 8 bits, drop 8 bits)
  | otherwise = failure bits
  where
    l = length bits

lol :: Bits
lol = Bits (packetToBits (Connect (Protocol "MQTT" 4) False 10 "a" Nothing Nothing))
