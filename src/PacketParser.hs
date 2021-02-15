module PacketParser where

import Bits
import CommonPacketParser
import MonadicParser
import Packets

connackParser :: Parser Bit Packet
connackParser = header connackPacketHeader body
  where
    body = do
      _ <- exacts (zeros 7)
      session <- next
      code <- connectReturnCode
      _ <- isAtEOF
      return (Connack session code)

connectParser :: Parser Bit Packet
connectParser = header connectPacketHeader body
  where
    body = do
      protocolName <- string
      _ <- check (protocolName == "MQTT")
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
