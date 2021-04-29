{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadConnect (
    MonadConnect,
    sendC,
    recvC,
    IOwithConnect,
    withConnect,
  ) where

import QueryResponse
import MonadDebug
import MonadBind
import Time
import Utils

import Data.ByteString.Char8 (pack)
import Data.Word (byteSwap32)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask, bracket)
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import qualified Network.DNS.Types as DT
import Network.DNS.Encode (encode)
import Network.DNS.Decode (decodeAt)
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as T
import System.Random

class Monad m => MonadConnect m where
    sendC :: Query -> m ()
    recvC :: m Response

newtype IOwithConnect a = IOC (ReaderT NS.Socket IOwithBind a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, Alternative, MonadPlus, MonadThrow, MonadCatch, MonadMask, MonadDebug, MonadBind)

instance MonadConnect IOwithConnect where
    sendC query@(Q domain qType) = IOC $ do
        debugTrace High $ "MonadConnect: > sendC: " ++ T.unpack (pShow query)
        sock <- ask
        identifier <- liftIO randomIO
        let quest = DT.Question (encodeDN domain) (encodeQT qType)
            msg = DT.makeQuery identifier quest mempty
        liftIO $ NBS.send sock (encode msg)
        debugTrace High "MonadConnect: < sendC"
        return ()
    recvC = IOC $ do
        debugTrace High "MonadConnect: > recvC"
        sock <- ask
        sa <- liftIO $ NS.getSocketName sock
        bs <- liftIO $ NBS.recv sock $ fromIntegral DT.maxUdpSize
        Time now <- currentTime
        msg <- decodeAt now bs ?: fail . show
        let [ans, auth, add] =
                ((decodeRR <$?>) . ($ msg)) <$> [DT.answer, DT.authority, DT.additional]
            response = R ans auth add
        debugTrace High $ "MonadConnect: < recvC: " ++ T.unpack (pShow response)
        return response

withConnect :: NS.HostAddress -> NS.PortNumber -> IOwithConnect a -> IOwithBind a
withConnect host port (IOC action) = do
    let open = liftIO $ NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
        close = liftIO . NS.close
        connect = (liftIO .) . NS.connect
    bracket open close $ \sock -> do
        connect sock $ NS.SockAddrInet port (byteSwap32 host)
        runReaderT action sock
