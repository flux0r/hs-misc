{-# LANGUAGE NoImplicitPrelude #-}

data Token t = Crlf t
             | Lws t
             | Token t
             | Separator t
             | Comment t
             | Ctext t
             | QuotedString t
             | Qdtext t
             | QuotedPair t
