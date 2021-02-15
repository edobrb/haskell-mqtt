module CommonPacketParser where

import Bits
import MonadicParser
import Packets
import qualified Codec.Binary.UTF8.String as UTF8

getInt :: Int -> Parser Bit Int
getInt n = do bits <- takes n; return (bitsToInt bits)

connectReturnCode :: Parser Bit ConnectReturnCode
connectReturnCode = do bits <- takes 8; get (toConnectReturnCode $ bitsToInt bits)

variableLength :: Parser Bit Int
variableLength = do
  c1 <- next
  b1 <- takes 7
  if c1
    then do
      c2 <- next
      b2 <- takes 7
      if c2
        then do
          c3 <- next
          b3 <- takes 7
          if c3
            then do
              c4 <- next
              b4 <- takes 7
              if c4
                then do empty
                else return $ bitsToInt (b4 ++ b3 ++ b2 ++ b1)
            else return $ bitsToInt (b3 ++ b2 ++ b1)
        else return $ bitsToInt (b2 ++ b1)
    else return $ bitsToInt b1

takeBodyBits :: Parser Bit [Bit]
takeBodyBits = do l <- variableLength; takes (l * 8)

header :: [Bit] -> Parser Bit Packet -> Parser Bit Packet
header fixed = (>->) h
  where
    h = do
      _ <- exacts fixed
      takeBodyBits

string :: Parser Bit String
string = do bits <- payloadData; return (UTF8.decode $ bitsToWords bits)

payloadData :: Parser Bit [Bit]
payloadData = do stringLength <- getInt 16; takes (stringLength * 8)

qosParser :: Parser Bit QoS
qosParser = qos0 <|> qos1 <|> qos2
  where
    qos0 = do _ <- exact zero; _ <- exact zero; return QoS0
    qos1 = do _ <- exact zero; _ <- exact one; return QoS1
    qos2 = do _ <- exact one; _ <- exact zero; return QoS2

parseIf :: Bool -> Parser e r -> Parser e (Maybe r)
parseIf False _ = continueWith Nothing
parseIf True parser = Just <$> parser