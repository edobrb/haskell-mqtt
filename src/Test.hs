module Test where

import PacketFactory
import PacketParser
import Packets

applicationMessage1 :: ApplicationMessage
applicationMessage1 = ApplicationMessage {retain = True, qos = QoS2, topic = "b", payload = [False, False, False, False, True, False, True, False, False, False, False, False, True, False, True, True]}

connectPacket1 :: Packet
connectPacket1 = Connect (Protocol "MQTT" 4) True 10 "hello" (Just $ Credential "user" (Just (map (const True) [0 .. 15]))) (Just applicationMessage1)

checkBuildAndParse :: Packet -> Bool
checkBuildAndParse packet = case parsePacket $ packetToBits packet of
  (p, _) : _ -> p == packet
  _ -> False
