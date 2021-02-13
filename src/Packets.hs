module Packets where

import Bits
import Data.Maybe
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
controlPacketType n = padTo 4 (bitsFrom n)

packetIdentifier :: Int -> [Bit]
packetIdentifier n = padTo 16 (bitsFrom n)

stringToBits :: String -> [Bit]
stringToBits s = [] ++ [] ++ []

qosToBits :: QoS -> [Bit]
qosToBits QoS0 = [zero, zero]
qosToBits QoS1 = [zero, one]
qosToBits QoS2 = [one, zero]

contains :: (Eq a) => Maybe a -> a -> Bool
contains (Just x) y = x == y
contains _ _ = False

packetToByte :: Packet -> Bits
packetToByte p@Connect {} =
  Bits (controlPacketType 1 ++ zeros 4 ++ remainingLength ++ packet)
  where
    remainingLength = toVariableLengthInteger (length packet `div` 8)
    packet = protocolName ++ protocolLevel ++ flags ++ keepAliveBits ++ clientIdBits ++ willTopicBits ++ willPayloadBits ++ usernameBits ++ passwordBits
    protocolName = stringToBits (name (protocol p))
    protocolLevel = padTo 8 (bitsFrom (level (protocol p)))
    usernameFlag = isJust (credentials p)
    passwordFlag = isJust (credentials p >>= password)
    willRetain = fmap retain (willMessage p) `contains` True
    willQoS = qosToBits (maybe QoS0 qos (willMessage p))
    willFlag = isJust (willMessage p)
    flags = [usernameFlag, passwordFlag, willRetain] ++ willQoS ++ [willFlag, cleanSession p, zero]
    keepAliveBits = padTo 16 (bitsFrom (keepAliveSeconds p))  
    clientIdBits = stringToBits (clientId p)
    willTopicBits = maybe [] (stringToBits . topic) (willMessage p)
    willPayloadBits = maybe [] payload (willMessage p)
    usernameBits = maybe [] (stringToBits . username) (credentials p)
    passwordBits = maybe [] stringToBits (credentials p >>= password)
    
packetToByte (Connack session code) =
  Bits (controlPacketType 2 ++ zeros 4 ++ remainingLength ++ sessionPresentBits ++ returnCodeBits)
  where
    remainingLength = padTo 8 (bitsFrom 2)
    --remainingLength = toVariableLengthInteger ((length sessionPresentBits + length returnCodeBits) `div` 8)
    sessionPresentBits = padTo 8 [session]
    returnCodeBits = padTo 8 (bitsFrom (fromEnum code))
packetToByte _ = Bits []

c :: Bits
c = packetToByte (Connack False ConnectionAccepted)
