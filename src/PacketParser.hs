module PacketParser where

import Bits
import CommonPacketParser
import MonadicParser
import Packets

connackParser :: Parser Bit Packet
connackParser = header 2 [zero, zero, zero, zero] body
  where
    body = do
      _ <- exacts (zeros 7)
      session <- next
      code <- connectReturnCode
      _ <- isAtEOF
      return (Connack session code)
