{-# LANGUAGE NoImplicitPrelude #-}

import Control.Category ((.), id)
import Data.Either (Either)
import Data.Maybe (Maybe (Nothing))
import Data.Primitive (ByteArray)
import Data.Text (Text)
import Data.Word (Word, Word8)
import Data.Vector (Vector)

data Bound = Bound
    { count     :: Word
    , lo        :: Word8
    , hi        :: Word8
    }

data Lexer s t = Lexer (Action s t) (Partial s t)

newtype Action s t = Action (Maybe (Transition s t))

data Partial s t = Top Bound (Vector (Lexer s t))
                 | Bottom Bound [(Word8, Lexer s t)]
                 | Done s t

data Result s t = Result
    { token     :: Either LexErr t
    , position  :: Position
    , lexer     :: Maybe (Lexer s t)
    }

newtype Transition s t = Transition (Buf -> Position -> s -> Result s t)

data LexErr = LexErr Text

data Position = Position
    { off   :: Word
    , len   :: Word
    }

newtype Buf = B { arr :: ByteArray }

newtype Pattern s t = Pattern (Lexer s t -> Lexer s t)

epsilon :: Pattern s t
epsilon = Pattern id

(++) :: Pattern s t -> Pattern s t -> Pattern s t
(Pattern k1) ++ (Pattern k2) = Pattern (k1 . k2)

is :: Word8 -> Pattern s t
is w = Pattern (\l -> Lexer (Action Nothing)
                            (Bottom (Bound 1 w w) [(w, l)]))
