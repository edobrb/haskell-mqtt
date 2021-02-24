module PacketParser (parsePacket) where 

import Bits
import CommonPacketParser
import MonadicParser
import Packets

-- | parse a sequence of bits into a packet and the remaining sequence
parsePacket :: [Bit] -> [(Packet, [Bit])]
parsePacket = parse packetParser

-- | parse input stream into bit chunk of one packet
nextPacket :: Parser Bit [Bit]
nextPacket = do fixed <- takes 8; body <- takeBodyBits; return (fixed ++ body)

-- | parse input stream into packet
packetParser :: Parser Bit Packet
packetParser = nextPacket >-> packetChunkParser

-- | convert a bit chunk into packet instance
packetChunkParser :: Parser Bit Packet
packetChunkParser = connectParser <|> connackParser

connackParser :: Parser Bit Packet
connackParser = do
  _ <- exacts connackPacketHeader
  _ <- exacts (zeros 7)
  session <- next
  code <- connectReturnCode
  _ <- isAtEOF
  return (Connack session code)

connectParser :: Parser Bit Packet
connectParser = do
  _ <- exacts connectPacketHeader
  protocolName <- string
  protocolLevel <- getInt 8
  usernameFlagF <- next
  passwordFlagF <- next
  willRetainF <- next
  willQoSF <- qosParser
  willFlagF <- next
  cleanSessionF <- next
  _ <- exact zero
  keepAlive <- getInt 16
  clientIdValue <- string
  willMessageV <- parseIf willFlagF $ willMessageParser willRetainF willQoSF
  credentialsV <- parseIf usernameFlagF $ credentialsParser passwordFlagF
  _ <- isAtEOF
  return (Connect (Protocol protocolName protocolLevel) cleanSessionF keepAlive clientIdValue credentialsV willMessageV)
  where
    willMessageParser r q = do t <- string; p <- payloadData; return (ApplicationMessage r q t p)
    credentialsParser hasPassword = do u <- string; p <- parseIf hasPassword payloadData; return (Credential u p)
