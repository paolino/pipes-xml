{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Pipes.XML.Token where

import Control.Lens hiding (deep, none)
import Control.Monad.Cont
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Pipes
import Protolude hiding (yield, toS)
import Protolude.Conv

-- | xml tokens
data Token
  = -- | tag starts
    Tin Text
  | -- |  end of attribute list
    TinC Text
  | -- | an atttribute
    Tattr Text ByteString
  | -- | tag ends
    Tout Text
  | -- | some text
    Ttext ByteString
  | -- | some CDATA -- not implemented
    Tcdata ByteString
  | Tcomment Text
  | Tpreproc Text ByteString
  | Tdtd Text (Either ByteString [ByteString])
  deriving (Show)

makePrisms ''Token

chunking ::
  (Show s, Show a, Functor m) =>
  -- | initial parser state
  s ->
  -- | output stream item parse
  (s -> Parser (s, [a])) ->
  Pipe ByteString a m ()
chunking s0 p = go (parse $ p s0)
  where
    go q = do
      let loop q' b = case q' b of
            Done r (s, xs) -> do
              traverse_ yield xs
              if not . B.null $ r
                then loop (parse $ p s) r
                else go $ parse $ p s
            f@(Partial _) -> go $ feed f
            x -> panic $ show x
      await >>= loop q

data StateP = Outside | Attring Text | Elements Text deriving (Show)

dtdElements :: Parser [ByteString]
dtdElements = do
  void $ char '['
  xs <- many dtdElement
  skipSpace
  void $ char ']'
  pure xs

dtdElement :: Parser ByteString
dtdElement = do
  skipSpace
  void $ string "<!"
  skipSpace
  parseString '>' <* char '>'

-- ts = traceShow

parseToken :: StateP -> Parser (StateP, [Token])
parseToken Outside =
  msum
    [ do
        void $ string "</"
        name <- parseString '>' <* char '>'
        pure (Outside, pure $ Tout $ T.strip $ toS name),
      do
        void $ string "<!--"
        skipSpace
        c <- T.decodeUtf8 <$> parseStringS "-->"
        pure (Outside, pure $ Tcomment c),
      do
        void $ string "<?"
        skipSpace
        n <- T.decodeUtf8 <$> parseString ' '
        skipSpace
        c <- parseStringS "?>"
        pure (Outside, pure $ Tpreproc n c),
      do
        void $ string "<!"
        skipSpace
        void $ string "DOCTYPE"
        skipSpace
        n <- T.decodeUtf8 <$> parseString ' '
        skipSpace
        xs <- Right <$> dtdElements -- <|>  Left <$> parseQuoted
        skipSpace
        void $ char '>'
        pure (Outside, pure $ Tdtd n xs),
      do
        void $ char '<'
        skipSpace
        (n, d :: Int) <- parseStringP (0 <$ char ' ' <|> 1 <$ char '>' <|> 2 <$ string "/>")
        let name = T.decodeUtf8 n
        case d of
          0 -> pure (Attring name, pure $ Tin name)
          1 -> pure (Outside, [Tin name, TinC name])
          2 -> pure (Outside, [Tin name, TinC name, Tout name])
          _ -> panic "parse token is partial",
      do
        t <- parseString '<'
        guard $ B.length t > 0
        '<' <- peekChar'
        pure (Outside, pure $ Ttext t)
    ]
parseToken (Attring name) = do
  skipSpace
  msum
    [ do
        () <$ char '>' <|> () <$ string "?>"
        pure (Outside, pure $ TinC name),
      do
        () <$ string "/>"
        pure (Outside, [TinC name, Tout name]),
      do
        attr <- takeWhile1 ('=' /=)
        void $ char '='
        skipSpace
        value <- parseQuoted
        pure
          ( Attring name,
            pure $ Tattr (T.strip $ T.decodeUtf8 attr) value
          )
    ]
parseToken _ = panic "parse token ins partial"

escape :: Parser Char
escape = char '\\' >> '"' <$ satisfy (== '"')

amp :: Parser Char
amp = do
  void $ char '&'
  x <-
    msum
      [ '<' <$ string "lt",
        '>' <$ string "gt",
        '&' <$ string "amp",
        '"' <$ string "quot",
        '\'' <$ string "apos"
      ]
  void $ char ';'
  pure x

character :: Char -> Parser Char
character c = amp <|> escape <|> satisfy (/= c)

characterS :: Parser a -> Parser ([Char], a)
characterS c =
  (mempty,) <$> c
    <|> first . (:) <$> (amp <|> escape <|> anyChar) <*> characterS c

parseString :: Char -> Parser ByteString
parseString c = B.pack <$> many (character c)

parseStringS :: ByteString -> Parser ByteString
parseStringS c = fst <$> parseStringP (string c)

parseStringP :: Parser a -> Parser (ByteString, a)
parseStringP c = first B.pack <$> characterS c

parseQuoted :: Parser ByteString
parseQuoted = do
  void $ char '"'
  x <- parseString '"'
  void $ char '"'
  pure x
