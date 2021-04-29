{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadBind (
    MonadBind,
    sendB,
    recvB,
    MetaData,
    IOwithBind,
    withBind,
  ) where

import QueryResponse
import MonadDebug
import Time
import Utils

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask, bracket)
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import qualified Network.DNS.Types as DT
import Network.DNS.Encode (encode)
import Network.DNS.Decode (decodeAt)

class Monad m => MonadBind m where
    sendB :: Response -> m ()
    recvB :: m Query

instance MonadBind m => MonadBind (ReaderT r m) where
    sendB = lift . sendB
    recvB = lift $ recvB

instance MonadBind m => MonadBind (StateT s m) where
    sendB = lift . sendB
    recvB = lift $ recvB

type MetaData = Maybe (NS.SockAddr, DT.DNSMessage)

newtype IOwithBind a = IOB (ReaderT NS.Socket (StateT MetaData IOwithDebug) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, Alternative, MonadPlus, MonadThrow, MonadCatch, MonadMask, MonadDebug)

instance MonadBind IOwithBind where
    sendB (R ans auth add) = IOB $ do
        sock <- ask
        (sa, msg) <- get ?^ fail . const
            "`sendB' can't be called before `recvB'"
        let msg' = msg {
                DT.answer     = encodeRR <$> ans,
                DT.authority  = encodeRR <$> auth,
                DT.additional = encodeRR <$> add
              }
        liftIO $ NBS.sendTo sock (encode msg') sa
        return ()
    recvB = IOB $ do
        sock <- ask
        (bs, sa) <- liftIO $ NBS.recvFrom sock 4096
        Time now <- currentTime
        msg <- decodeAt now bs ?: fail . show
        let query = head $ decodeQ <$?> DT.question msg
        put $ Just (sa, msg)
        return query

withBind :: NS.PortNumber -> IOwithBind a -> IOwithDebug a
withBind port (IOB action) = do
    let open = liftIO $ NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
        close = liftIO . NS.close
        bind = (liftIO .) . NS.bind
    res <- bracket open close $ \sock -> do
        bind sock $ NS.SockAddrInet port 0x0100007f
        evalStateT (runReaderT action sock) Nothing
    return res
