module Packets where

import Bits
import Data.List.Split

data ConnectReturnCode
  = ConnectionAccepted
  | UnacceptableProtocolVersion
  | IdentifierRejected
  | ServerUnavailable
  | BadUsernameOrPassword
  | NotAuthorized
  deriving (Enum, Eq, Show)

data QoS = QoS0 | QoS1 | Qos2 deriving (Enum, Eq, Show)

data Protocol = Protocol {name :: String, level :: Int} deriving (Eq, Show)

data Credential = Credential {username :: String, password :: Maybe String} deriving (Eq, Show)

data ApplicationMessage = ApplicationMessage {retain :: Bool, qos :: QoS, topic :: String, payload :: Bits} deriving (Eq, Show)

data Packet
  = Connect
      { protocol :: Protocol,
        cleanSession :: Bool,
        keepAliveMs :: Int,
        clientId :: String,
        credentials :: Maybe Credential,
        willMessage :: ApplicationMessage
      }
  | Connack {sessionPresent :: Bool, returnCode :: ConnectReturnCode}
  | Puback {packedID :: Int}
  | Pubrec {packedID :: Int}
  deriving (Eq, Show)

packetToByte :: Packet -> Bits
packetToByte (Connack session code) =
  Bits (controlPacketType ++ zeros 4 ++ remainingLength ++ sessionPresentBits ++ returnCodeBits)
  where
    controlPacketType = padTo 4 (bitsFrom 2)
    remainingLength = padTo 8 (bitsFrom 2)
    --remainingLength = toVariableLengthInteger ((length sessionPresentBits + length returnCodeBits) `div` 8)
    sessionPresentBits = padTo 8 [session]
    returnCodeBits = padTo 8 (bitsFrom (fromEnum code))
packetToByte _ = Bits []

c :: Bits
c = packetToByte (Connack False ConnectionAccepted)

toVariableLengthInteger :: Int -> [Bit]
toVariableLengthInteger n
  | d == 0 = padTo 8 (bitsFrom r)
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
