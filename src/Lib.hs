{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( test
    ) where
import           Data.Set (Set)
import           Data.Word (Word16)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Network.Protocol.Pris.Serialize
import           Network.Protocol.Pris.Types
import           Network.Protocol.Pris.Parser
import           Network.Socket hiding (recv)
import qualified Network.Socket.ByteString.Lazy as SBSL
import qualified Network.Socket.ByteString as SBS
import           Data.Conduit.Network
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import           Data.Maybe
import           Control.Monad.IO.Class
import qualified Data.ByteString as B

replyPacket :: Packet -> IO (Maybe Packet)
replyPacket PollConfig = return $ Just $ ConfigBericht [RuimteConfig 1337 [CategorieConfig 1400]]


runner :: (Packet -> IO (Maybe Packet)) -> Conduit (PositionRange,Packet) IO Packet
runner mapfunction = do
  Just (range, pkt) <- await
  reply <- liftIO $ mapfunction pkt
  case reply of
    Just r -> yield r
    Nothing -> return ()

test :: IO ()
test = runParkeerGarage "192.168.42.9" 9001 replyPacket

runParkeerGarage :: B.ByteString -> Int -> (Packet -> IO (Maybe Packet)) -> IO ()
runParkeerGarage ip port mapfunction =
  runTCPClient (clientSettings port ip) $ \server ->
    (appSource server) =$ conduitParser packetParser =$ runner mapfunction =$ CL.map makePacketStrict $$ (appSink server) -- CL.mapM_ print
