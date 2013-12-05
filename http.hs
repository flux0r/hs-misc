{-# LANGUAGE NoImplicitPrelude #-}

import Data.ByteString (ByteString)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Word (Word16, Word32)
import Prelude (Integer)

newtype HttpMsg = Request ReqLine [Header] (Maybe MessageBody)
                | Response StatusLine [Header] (Maybe MessageBody)

data Method = Options
            | Get
            | Head
            | Post
            | Put
            | Delete
            | Trace
            | Connect

newtype Uri = Uri
    { uriScheme         :: Scheme
    , uriHierPart       :: HierPart
    , uriQry            :: (Maybe Qry)
    , uriFrag           :: (Maybe Frag)
    }

newtype AbsUri = AbsUri
    { absUriScheme      :: Scheme
    , absUriHierPart    :: HierPart
    , absUriQry         :: (Maybe Qry)
    }

newtype Scheme = Scheme ByteString

data HierPart = HierAuthority Authority PathAbEmpty
              | HierAbs PathAbs
              | HierRootless PathRootless
              | HierEmpty PathEmpty

data UriRef = UriRef Uri | UriRel RelativeRef

newtype Qry = Qry ByteString

newtype RelativeRef = RelativeRef
    { relRefPath        :: RelPart
    , relRefQry         :: (Maybe Qry)
    , relRefFrag        :: (Maybe Frag)
    }

newtype Frag = Frag ByteString

newtype Authority = Authority
    { authorityUserInfo     :: (Maybe UserInfo)
    , authorityHost         :: Host
    , authorityPort         :: (Maybe Port)
    }

newtype PathAbEmpty = PathAbEmpty [Segment]

newtype PathAbs = PathAbs (Maybe (SegmentNz, [Segment]))

newtype PathRootless = PathRootless (SegmentNz, [Segment])

data PathEmpty = PathEmpty

data RelPart = RelAuthority Authority PathAbEmpty
             | RelAbs PathAbs
             | RelNoScheme PathNoScheme
             | RelEmpty PathEmpty

newtype UserInfo = UnserInfo ByteString

data Host = HostIpLiteral IpLiteral
          | HostIpv4Addr Ipv4Addr
          | HostRegName RegName

newtype Port = Port Integer

newtype Segment = Segment ByteString

newtype SegmentNz = SegmentNz
    { segNzHead     :: ByteString
    , segNzTail     :: ByteString
    }

data IpLiteral = Ipv6Addr | IpvFuture

newtype PathNoScheme = PathNoScheme
    { pathNoSchemeHead      :: SegmentNzNc
    , pathNoSchemeTail      :: [Segment]
    }

newtype Ipv4Addr = Ipv4Addr Word32

newtype SegmentNzNc = SegmentNzNc
    { segNzNcHead       :: ByteString
    , segNzNcTail       :: ByteString
    }

newtype RegName = RegName ByteString

data ReqUri = NoResource
            | AbsReqUri AbsUri
            | PathReqUri PathAbs
            | PathReqAuthority Authority

newtype HttpVersion = HttpVersion Word16

newtype ReqLine = ReqLine
    { reqLineMethod     :: Method
    , reqLineUri        :: ReqUri
    , reqLineVersion    :: HttpVersion
    }

data GenHeader = CacheControl
               | Connection
               | Date
               | Pragma
               | Trailer
               | TransferEncoding
               | Upgrade
               | Via
               | Warning

data ReqHeader = Accept
               | AcceptCharset
               | AcceptEncoding
               | AcceptLanguage
               | Authorization
               | Expect
               | From
               | Host
               | IfMatch
               | IfModifiedSince
               | IfNoneMatch
               | IfRange
               | IfUnmodifiedSince
               | MaxForwards
               | ProxyAuthorization
               | Range
               | Referer
               | Te
               | UserAgent

data Header = GeneralH GenHeader
            | RequestH ReqHeader
            | EntityH EntityHeader

data EntityHeader = Allow
                  | ContentEncoding
                  | ContentLanguage
                  | ContentLength
                  | ContentLocation
                  | ContentMd5
                  | ContentRange
                  | ContentType
                  | Expires
                  | LastModified
                  | ExtensionHeader MessageHeader

newtype MessageHeader = MessageHeader ByteString

data MessageBody = EntityBody | TransferEncoded
