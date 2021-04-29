module Time (
    Time (Time),
    TTL (TTL),
    TOD (TOD),
    mkTOD,
    currentTime,
  ) where

import Data.Int (Int64)
import Data.Word (Word32)
import Time.System (timeCurrent)
import Time.Types (Elapsed(..), Seconds(..))
import Control.Monad.IO.Class (MonadIO, liftIO)

newtype Time = Time Int64
    deriving (Eq, Ord, Show)

newtype TTL = TTL Word32
    deriving Show

newtype TOD = TOD Time
    deriving Show

mkTOD :: Time -> TTL -> TOD
mkTOD (Time x) (TTL y) = TOD (Time (x + fromIntegral y))

currentTime :: MonadIO m => m Time
currentTime = do
    Elapsed (Seconds time) <- liftIO $ timeCurrent
    return $ Time time
