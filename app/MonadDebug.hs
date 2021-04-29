{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadDebug (
    DebugLevel (..),
    MonadDebug,
    setDebugLevel,
    debugTrace,
    IOwithDebug,
    withDebug,
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)

data DebugLevel =
      Low
    | Middle
    | High
    deriving (Eq, Ord)

class Monad m => MonadDebug m where
    setDebugLevel :: Maybe DebugLevel -> m ()
    debugTrace :: DebugLevel -> String -> m ()

instance MonadDebug m => MonadDebug (ReaderT r m) where
    setDebugLevel = lift . setDebugLevel
    debugTrace = (lift .) . debugTrace

instance MonadDebug m => MonadDebug (StateT s m) where
    setDebugLevel = lift . setDebugLevel
    debugTrace = (lift .) . debugTrace

newtype IOwithDebug a = IOD (StateT (Maybe DebugLevel) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, Alternative, MonadPlus, MonadThrow, MonadCatch, MonadMask)

instance MonadDebug IOwithDebug where
    setDebugLevel = IOD . put
    debugTrace dl str = IOD $ do
        mdl <- get
        if mdl < Just dl
        then return ()
        else liftIO $ putStrLn str

withDebug :: Maybe DebugLevel -> IOwithDebug a -> IO a
withDebug mdl (IOD action) = evalStateT action mdl
