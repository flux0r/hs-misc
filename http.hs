{-# LANGUAGE NoImplicitPrelude #-}

import Data.ByteString (ByteString)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Word (Word16, Word32)
import Prelude (Integer)

data HttpMsg = Request ReqLine [Header] (Maybe MessageBody)
             | Response StatusLine [Header] (Maybe MessageBody)

data Method = Options
            | Get
            | Head
            | Post
            | Put
            | Delete
            | Trace
            | Connect

data Uri = Uri
    { uriScheme         :: Scheme
    , uriHierPart       :: HierPart
    , uriQry            :: (Maybe Qry)
    , uriFrag           :: (Maybe Frag)
    }

data AbsUri = AbsUri
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

data RelativeRef = RelativeRef
    { relRefPath        :: RelPart
    , relRefQry         :: (Maybe Qry)
    , relRefFrag        :: (Maybe Frag)
    }

newtype Frag = Frag ByteString

data Authority = Authority
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

data SegmentNz = SegmentNz
    { segNzHead     :: ByteString
    , segNzTail     :: ByteString
    }

data IpLiteral = Ipv6Addr | IpvFuture

data PathNoScheme = PathNoScheme
    { pathNoSchemeHead      :: SegmentNzNc
    , pathNoSchemeTail      :: [Segment]
    }

newtype Ipv4Addr = Ipv4Addr Word32

data SegmentNzNc = SegmentNzNc
    { segNzNcHead       :: ByteString
    , segNzNcTail       :: ByteString
    }

newtype RegName = RegName ByteString

data ReqUri = NoResource
            | AbsReqUri AbsUri
            | PathReqUri PathAbs
            | PathReqAuthority Authority

newtype HttpVersion = HttpVersion Word16

data ReqLine = ReqLine
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

data StatusLine = StatusLine
    { statLineVersion       :: HttpVersion
    , statLineCode          :: StatusCode
    , statLineReason        :: ReasonPhrase
    }

data StatusCode = Informational InformationalCode
                | Success SuccessCode
                | Redirection RedirectionCode
                | ClientErr ClientErrCode
                | ServErr ServErrCode

data InformationalCode = Cont
                       | SwitchingProtocols

data SuccessCode = Ok
                 | Created
                 | Accepted
                 | NonAuthoritativeInformation
                 | NoContent
                 | ResetContent
                 | PartialContent

data RedirectionCode = MultipleChoices
                     | MovedPermanently
                     | Found
                     | SeeOther
                     | NotModified
                     | UseProxy
                     | TemporaryRedirect

data ClientErrCode = BadRequest
                   | Unauthorized
                   | PaymentRequired
                   | Forbidden
                   | NotFound
                   | MethodNotAllowed
                   | NotAcceptable
                   | ProxyAuthenticationRequired
                   | RequestTimeOut
                   | Conflict
                   | Gone
                   | LengthRequired
                   | PreconditionFailed
                   | RequestEntityTooLarge
                   | RequestUriTooLarge
                   | UnsupportedMediaType
                   | RequestedRangeNotSatisfiable
                   | ExpectationFailed

data ServErrCode = InternalServerError
                 | NotImplemented
                 | BadGateway
                 | ServiceUnavailable
                 | GatewayTimeout
                 | HttpVersionNotSupported

newtype ReasonPhrase = ReasonPhrase ByteString
