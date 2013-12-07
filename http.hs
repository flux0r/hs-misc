{-# LANGUAGE MagicHash, NoImplicitPrelude #-}

import Data.Bool ((||), (&&), Bool (True, False))
import Data.ByteString (ByteString)
import Data.Either (Either)
import Data.Eq ((==))
import Data.Maybe (Maybe)
import Data.Ord ((<), (<=))
import Data.Primitive (ByteArray)
import Data.Word (Word, Word8, Word16, Word32)
import Prelude (Integer)

data HttpMsg = Request ReqLine [ReqHeader] (Maybe MessageBody)
             | Response StatusLine [ResHeader] (Maybe MessageBody)

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

data RequestHeader = Accept
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

data ResponseHeader = AcceptRanges
                    | Age
                    | ETag
                    | Location
                    | ProxyAuthenticate
                    | RetryAfter
                    | Server
                    | Vary
                    | WwwAuthenticate

data ReqHeader = ReqGeneralH GenHeader
               | RequestH RequestHeader
               | ReqEntityH EntityHeader

data ResHeader = ResGeneralH GenHeader
               | ResponseH ResponseHeader
               | ResEntityH EntityHeader

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

is_octet, is_char, is_upalpha,
    is_loalpha, is_alpha, is_digit, is_ctl,
    is_cr, is_lf, is_sp, is_ht, is_dbl_quot :: Word8 -> Bool
is_octet _      = True
is_char x       = x < 0x80
is_upalpha x    = 0x40 < x && x < 0x5b
is_loalpha x    = 0x60 < x && x < 0x7b
is_alpha x      = is_upalpha x || is_loalpha x
is_digit x      = 0x2f < x && x < 0x3a
is_ctl x        = x < 0x20 || x == 0x7f
is_cr           = (== 0x0d)
is_lf           = (== 0x0a)
is_sp           = (== 0x20)
is_ht           = (== 0x09)
is_dbl_quot     = (== 0x22)

is_crlf :: Word16 -> Bool
is_crlf = (== 0x0d0a)

data Buf = B
    { arr       :: ByteArray
    , off       :: Word
    , len       :: Word
    }
