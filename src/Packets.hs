module Packets where

import Bits

data ConnectReturnCode
  = ConnectionAccepted
  | UnacceptableProtocolVersion
  | IdentifierRejected
  | ServerUnavailable
  | BadUsernameOrPassword
  | NotAuthorized
  deriving (Enum, Eq, Show)

toConnectReturnCode :: Int -> Maybe ConnectReturnCode
toConnectReturnCode 0 = Just ConnectionAccepted
toConnectReturnCode 1 = Just UnacceptableProtocolVersion
toConnectReturnCode 2 = Just IdentifierRejected
toConnectReturnCode 3 = Just ServerUnavailable
toConnectReturnCode 4 = Just BadUsernameOrPassword
toConnectReturnCode 5 = Just NotAuthorized
toConnectReturnCode _ = Nothing

data QoS = QoS0 | QoS1 | QoS2 deriving (Enum, Eq, Show)

data Protocol = Protocol {name :: String, level :: Int} deriving (Eq, Show)

data Credential = Credential {username :: String, password :: Maybe [Bit]} deriving (Eq, Show)

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

connectPacketHeader :: [Bit]
connectPacketHeader = controlPacketType 1 ++ zeros 4

connackPacketHeader :: [Bit]
connackPacketHeader = controlPacketType 2 ++ zeros 4

