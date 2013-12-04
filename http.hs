{-# LANGUAGE NoImplicitPrelude #-}

import Data.ByteString (ByteString)
import Data.Either (Either)
import Data.Maybe (Maybe)

data HttpMsg = Request | Response

data Method = Options
            | Get
            | Head
            | Post
            | Put
            | Delete
            | Trace
            | Connect

data Uri = Uri Scheme HierPart (Maybe Qry) (Maybe Frag)

newtype Scheme = Scheme ByteString

data HierPart = HierAuthority Authority PathAbEmpty
              | HierPathAbsolute PathAbsolute
              | HierPathRootless PathRootless
              | HierPathEmpty PathEmpty

data UriRef = Either Uri RelativeRef

newtype Qry = Qry ByteString

data RelativeRef = RelativeRef RelativePart (Maybe Qry) (Maybe Frag)

newtype Frag = Frag ByteString

data Authority = Authority (Maybe UserInfo) Host (Maybe Port)

newtype PathAbEmpty = PathAbEmpty [Segment]

newtype PathAbsolute = PathAbsolute (Maybe (SegmentNz, [Segment]))

newtype PathRootless = PathRootless (SegmentNz, [Segment])

data PathEmpty = PathEmpty

data RelativePart = RelativeAuthority Authority PathAbEmpty
                  | RelPathAbsolute PathAbsolute
                  | RelPathNoScheme PathNoScheme
                  | RelPathEmpty PathEmpty

newtype UserInfo = UnserInfo ByteString
