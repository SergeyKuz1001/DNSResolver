{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadStorage (
    MonadStorage,
    load,
    save,
    clear,
    IOwithStorage,
    withStorage,
    defaultStorage,
  ) where

import QueryResponse
import MonadDebug
import MonadBind
import MonadConnect
import Time
import Utils

import Control.Monad.State (StateT, evalStateT, get, put, modify, gets)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.IP (fromIPv4w, toIPv4)
import Control.Monad (forM_, unless)
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as T

class Monad m => MonadStorage m where
    load :: Query -> m Answers
    save :: Answers -> m ()
    clear :: DomainName -> m ()

data StorageRecord = SR RRTypeAndData (Maybe (TOD, TTL))

instance Eq StorageRecord where
    (SR tad1 _) == (SR tad2 _) = tad1 == tad2

instance Ord StorageRecord where
    (SR tad1 _) <= (SR tad2 _) = tad1 <= tad2

sr2rr :: DomainName -> StorageRecord -> ResourceRecord
sr2rr domain (SR tad Nothing) = RR domain tad (TTL 3600)
sr2rr domain (SR tad (Just (_, ttl))) = RR domain tad ttl

isNotObsoleteSR :: Time -> StorageRecord -> Bool
isNotObsoleteSR time (SR _ Nothing) = True
isNotObsoleteSR time (SR _ (Just (TOD time', _))) = time < time'

type Storage = Map.Map DomainName (Set.Set StorageRecord)

newtype IOwithStorage a = IOS { unIOS :: (StateT Storage IOwithBind a) }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, Alternative, MonadPlus, MonadThrow, MonadCatch, MonadMask, MonadDebug, MonadBind)

instance MonadStorage IOwithStorage where
    load query@(Q domain qType) = IOS $ do
        debugTrace High "MonadStorage: load"
        unIOS $ clear domain
        storage <- get
        case domain `Map.lookup` storage of
            Just srs -> do
                debugTrace High $ "MonadStorage: load: " ++ show domain ++ " in cache"
                return $ getRRwithQType qType <$?> (sr2rr domain <$> Set.toList srs)
            Nothing  -> do
                debugTrace High $ "MonadStorage: load: " ++ show domain ++ " not in cache"
                nss <- fmap (getNS <$?>) $ unIOS $ load $ Q (subdomain domain) Q_NS
                debugTrace High $ "MonadStorage: load: nss = " ++ T.unpack (pShow nss)
                tryForAll nss $ \ns -> do
                    ips <- fmap (getIP <$?>) $ unIOS $ load $ Q ns Q_A
                    debugTrace High $ "MonadStorage: load: ips = " ++ T.unpack (pShow ips)
                    tryForAll ips $ \ip -> do
                        debugTrace High $ "MonadStorage: load: withConnect: " ++ show ip ++ " 53"
                        R ans auth add <-
                            lift $ withConnect (fromIPv4w ip) 53 $ do
                                sendC query
                                recvC
                        unIOS $ save $ ans ++ auth ++ add
                        unIOS $ load query
                        --return $ getRRwithQType qType <$?> ans ++ auth ++ add
    save anss = IOS $ do
        debugTrace High $ "MonadStorage: save " ++ T.unpack (pShow anss)
        now <- currentTime
        forM_ anss $
            \(RR domain tad ttl) -> do
                hasDomain <- gets (Map.member domain)
                unless hasDomain $
                    modify $ Map.insert domain Set.empty
                modify $ flip Map.adjust domain $
                    (Set.insert (SR tad $ Just (mkTOD now ttl, ttl)))
    clear domain = IOS $ do
        debugTrace High $ "MonadStorage: clear " ++ show domain
        now <- currentTime
        modify $ flip Map.adjust domain $ Set.filter $ isNotObsoleteSR now

withStorage :: Storage -> IOwithStorage a -> IOwithBind a
withStorage storage (IOS action) = evalStateT action storage

defaultStorage = Map.fromList [
    (DN [], Set.fromList [
        SR (RR_NS $ DN ["a","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["b","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["c","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["d","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["e","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["f","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["g","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["h","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["i","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["j","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["k","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["l","root-servers","net"]) Nothing,
        SR (RR_NS $ DN ["m","root-servers","net"]) Nothing
      ]),
    (DN ["a","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [198,41,0,4])) Nothing
      ]),
    (DN ["b","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [199,9,14,201])) Nothing
      ]),
    (DN ["c","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [192,33,4,12])) Nothing
      ]),
    (DN ["d","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [199,7,91,13])) Nothing
      ]),
    (DN ["e","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [192,203,230,10])) Nothing
      ]),
    (DN ["f","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [192,5,5,241])) Nothing
      ]),
    (DN ["g","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [192,112,36,4])) Nothing
      ]),
    (DN ["h","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [198,97,190,53])) Nothing
      ]),
    (DN ["i","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [192,36,148,17])) Nothing
      ]),
    (DN ["j","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [192,58,128,30])) Nothing
      ]),
    (DN ["k","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [193,0,14,129])) Nothing
      ]),
    (DN ["l","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [199,7,83,42])) Nothing
      ]),
    (DN ["m","root-servers","net"], Set.fromList [
        SR (RR_A (toIPv4 [202,12,27,33])) Nothing
      ])
  ]
