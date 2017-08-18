{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Protocol.Pris.Serialize
  (PrisSerialize, makePacket, makePacketStrict)
  where

import           Data.Set (Set)
import           Data.ByteString.Builder
import           Network.Protocol.Pris.Types
import qualified Data.Map.Strict as Map
import           Data.Word (Word16)
import           Data.Monoid ((<>))
import           Data.Map.Strict (Map)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import           Data.Bits
import           Network.Protocol.Pris.CRC16
class PrisSerialize a where
  serialize :: a -> Builder
  default serialize :: Enum a => a -> Builder
  serialize x = word16BE $ toEnum $ fromEnum x

instance PrisSerialize Word16 where
  serialize = word16BE 

instance Enum x => PrisSerialize (Set x) where
  serialize = word16BE . (foldr (flip setBit . fromEnum) 0)

instance PrisSerialize y => PrisSerialize (Map Word16 y) where
  serialize x = word16BE (fromIntegral $ Map.size x) <>
                Map.foldMapWithKey addmap x
    where addmap key val = word16BE key <> serialize val

instance PrisSerialize y => PrisSerialize [y] where
  serialize x = word16BE (fromIntegral $ length x) <>
                foldMap (serialize) x

instance PrisSerialize ParkeerStatus

instance PrisSerialize CategorieStatus where
  serialize CategorieStatus{..} =
    word16BE aantalParkeerders <>
    word16BE aantalInrijders <>
    word16BE aantalUitrijders

instance PrisSerialize (t CategorieStatus) => PrisSerialize (RuimteStatus t) where
  serialize RuimteStatus{..} =
    serialize ruimteStatus <>
    serialize storingsToestand <>
    serialize categorien
instance PrisSerialize (t CategorieConfig) => PrisSerialize (RuimteConfig t) where
  serialize RuimteConfig{..} =
    serialize capaciteitRuimte <>
    serialize ruimteCategorien
instance PrisSerialize CategorieConfig where
  serialize CategorieConfig{..} = serialize capaciteitParkeerders

toPacketId :: Packet -> Word16
toPacketId PollConfig = 0x0001
toPacketId PollStatus = 0x0002
toPacketId (WijzigStatus _) = 0x0003
toPacketId (WijzigConfig _) = 0x0004
toPacketId (ConfigBericht _) = 0x0081
toPacketId (StatusBericht _) = 0x0082

toPayload :: Packet -> Builder
toPayload (PollConfig) = mempty
toPayload (PollStatus) = mempty
toPayload (WijzigStatus x) = serialize x
toPayload (WijzigConfig x) = serialize x
toPayload (ConfigBericht x) = serialize x
toPayload (StatusBericht x) = serialize x

makePacket :: Packet -> L.ByteString
makePacket packet = L.concat [toLazyByteString headchk, introdata, toLazyByteString tail]
  where
    payload = toPayload packet
    packetid = toPacketId packet
    intro = word8 2 <> word8 0 <> word16BE 101 <> (word16BE packetid)
    tail = char8 '\r'
    introdata = toLazyByteString $ (intro <> payload)
    len = 6 + L.length (introdata)
    head = word8 0xe3 <> word16BE (fromIntegral len) <> word16BE (crc16_ANSI introdata)
    headchk = head <> (word8 $ L.foldl1' xor $ toLazyByteString head)
  
makePacketStrict :: Packet -> B.ByteString
makePacketStrict = B.concat . L.toChunks . makePacket
