module QueryResponse (
    DomainName (DN),
    subdomain,
    encodeDN,
    decodeDN,
    QType (Q_A, Q_NS),
    encodeQT,
    decodeQT,
    Query (Q),
    encodeQ,
    decodeQ,
    NameServer,
    RRTypeAndData (RR_A, RR_NS),
    ResourceRecord (RR),
    getIP,
    getNS,
    getQTypeOfRR,
    getRRwithQType,
    encodeRR,
    decodeRR,
    Answers,
    AuthorityRecords,
    AdditionalRecords,
    Response (R),
  ) where

import Time

import Data.IP (IPv4)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Network.DNS.Types as DT

newtype DomainName = DN [String]
    deriving (Eq, Ord)

instance Show DomainName where
    show (DN []) = ""
    show (DN [part]) = part
    show (DN (part:parts)) = part ++ "." ++ show (DN parts)

subdomain :: DomainName -> DomainName
subdomain (DN parts) = DN (tail parts)

encodeDN :: DomainName -> DT.Domain
encodeDN (DN parts) = BS.pack $ concat $ map (++ ".") parts

decodeDN :: DT.Domain -> DomainName
decodeDN domain = DN $ go $ BS.unpack domain where
    go :: String -> [String]
    go "" = []
    go str =
        let (strPart, strRest) = break (== '.') str
        in  case strRest of
                "" -> [strPart]
                dot:strRest' -> strPart : go strRest'

data QType =
      Q_A
    | Q_NS
    deriving (Show, Eq)

encodeQT :: QType -> DT.TYPE
encodeQT Q_A  = DT.A
encodeQT Q_NS = DT.NS

decodeQT :: DT.TYPE -> Maybe QType
decodeQT DT.A  = Just Q_A
decodeQT DT.NS = Just Q_NS
decodeQT _ = Nothing

data Query = Q DomainName QType
    deriving Show

encodeQ :: Query -> DT.Question
encodeQ (Q domain Q_A)  = DT.Question (encodeDN domain) DT.A
encodeQ (Q domain Q_NS) = DT.Question (encodeDN domain) DT.NS

decodeQ :: DT.Question -> Maybe Query
decodeQ (DT.Question domain DT.A)  = Just $ Q (decodeDN domain) Q_A
decodeQ (DT.Question domain DT.NS) = Just $ Q (decodeDN domain) Q_NS
decodeQ _ = Nothing

type NameServer = DomainName

data RRTypeAndData =
      RR_A IPv4
    | RR_NS NameServer
    deriving (Eq, Ord, Show)

data ResourceRecord = RR DomainName RRTypeAndData TTL
    deriving Show

getIP :: ResourceRecord -> Maybe IPv4
getIP (RR _ (RR_A ip) _) = Just ip
getIP _ = Nothing

getNS :: ResourceRecord -> Maybe NameServer
getNS (RR _ (RR_NS ns) _) = Just ns
getNS _ = Nothing

getQTypeOfRR :: ResourceRecord -> QType
getQTypeOfRR (RR _ (RR_A  _) _) = Q_A
getQTypeOfRR (RR _ (RR_NS _) _) = Q_NS

getRRwithQType :: QType -> ResourceRecord -> Maybe ResourceRecord
getRRwithQType qType rr =
    if getQTypeOfRR rr == qType
    then Just rr
    else Nothing

encodeRR :: ResourceRecord -> DT.ResourceRecord
encodeRR (RR domain (RR_A ip) (TTL ttl)) =
    DT.ResourceRecord (encodeDN domain) DT.A DT.classIN ttl (DT.RD_A ip)
encodeRR (RR domain (RR_NS domain') (TTL ttl)) =
    DT.ResourceRecord (encodeDN domain) DT.NS DT.classIN ttl (DT.RD_NS (encodeDN domain'))

decodeRR :: DT.ResourceRecord -> Maybe ResourceRecord
decodeRR (DT.ResourceRecord domain DT.A _ ttl (DT.RD_A ip)) =
    Just $ RR (decodeDN domain) (RR_A ip) (TTL ttl)
decodeRR (DT.ResourceRecord domain DT.NS _ ttl (DT.RD_NS domain')) =
    Just $ RR (decodeDN domain) (RR_NS (decodeDN domain')) (TTL ttl)
decodeRR _ = Nothing

type Answers = [ResourceRecord]

type AuthorityRecords = [ResourceRecord]

type AdditionalRecords = [ResourceRecord]

data Response = R Answers AuthorityRecords AdditionalRecords
    deriving Show
