{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Protocol.Pris.Parser
(parsePacket, packetParser)
where

import           Network.Protocol.Pris.Types
import           Data.Attoparsec.ByteString hiding (parse)
import qualified Data.Attoparsec.ByteString as P
import           Data.Word (Word16)
import           Data.Attoparsec.Binary
import qualified Data.ByteString as BS
import           Control.Applicative ((<*>))
import qualified Data.Map.Strict as Map
import           Data.Bits
import           Data.Set (Set)
import           Data.Map.Strict (Map)
import           Control.Monad (foldM)
import qualified Data.Set as Set

parsePacket :: BS.ByteString -> Result Packet
parsePacket x = P.feed (P.parse packetParser x) (BS.pack [])

class PrisParse a where
  parse :: Parser a
  default parse :: (Bounded a, Enum a) => Parser a
  -- todo: safety
  parse = toEnum <$> fromIntegral <$> anyWord16be
  
instance PrisParse Word16 where
  parse = anyWord16be

instance PrisParse y => PrisParse (Map Word16 y) where
  parse = do
    num <- anyWord16be
    foldM readEntry Map.empty [Prelude.take (fromIntegral num) $ repeat 0]
    where
      readEntry map _ = do
        key <- anyWord16be
        val <- parse
        return $ Map.insert key val map

instance PrisParse y => PrisParse ([y]) where
  parse = do
    num <- anyWord16be
    mapM (const $ parse) (Prelude.take (fromIntegral num) $ repeat 0)
instance (Enum t, Bounded t, Ord t) => PrisParse (Set t) where
  parse = do
    x <- anyWord16be
    return $ Set.fromList $ filter (testBit x . fromEnum) ([minBound..maxBound])
instance PrisParse ParkeerStatus
instance PrisParse CategorieStatus where
  parse = CategorieStatus <$> parse <*> parse <*> parse
instance PrisParse (t CategorieStatus) => PrisParse (RuimteStatus t) where
  parse = RuimteStatus <$> parse <*> parse <*> parse

instance PrisParse (t CategorieConfig) => PrisParse (RuimteConfig t) where
  parse = RuimteConfig <$> parse <*> parse

instance PrisParse CategorieConfig where
  parse = CategorieConfig <$> parse

headParser :: Parser Word16
headParser = do
  word8 0xe3
  len <- anyWord16be
  crc16 <- anyWord16be
  hdchk <- anyWord8
  word8 2 >> word8 0 >> word16be 101
  packetid <- anyWord16be
  return packetid
  
packetParser :: Parser Packet
packetParser = do
  packetid <- headParser
  payload <- case packetid of
    0x0001 -> return PollConfig
    0x0002 -> return PollStatus
    0x0003 -> WijzigStatus <$> parse
    0x0004 -> WijzigConfig <$> parse
    0x0081 -> ConfigBericht <$> parse
    0x0082 -> StatusBericht <$> parse
  word8 13
  return payload
