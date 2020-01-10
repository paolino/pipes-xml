{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Pipes.XML.Token where

import           Control.Arrow                  ( (***) )
import           Control.Lens            hiding ( deep
                                                , none
                                                )
import           Control.Lens.Extras
import Control.Monad.Cont
import           Data.Map                       ( Map, insert )
import           Protolude              
import           Pipes
import           Pipes.Core
import           Pipes.Safe              hiding ( handle )
import qualified Pipes.Prelude as P
import Data.Attoparsec.ByteString.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B

-- | xml tokens
data Token = Tin Text -- ^ tag starts
           | TinC Text -- ^  end of attribute list
           | Tattr Text ByteString -- ^ an atttribute
           | Tout Text -- ^ tag ends
           | Ttext ByteString -- ^ some text
           | Tcdata ByteString -- ^ some CDATA -- not implemented
    deriving Show

makePrisms ''Token

chunking
    :: (Show s, Show a, Functor m)
    => s -- ^ initial parser state
    -> (s -> Parser (s, [a])) -- ^ output stream item parse
    -> Pipe ByteString a m ()
chunking s0 p = go (parse $ p s0)
  where
    go q = do
        let loop q b = case q b of
                Done r (s, xs) -> do
                    traverse_ yield xs
                    if not . B.null $ r
                        then loop (parse $ p s) r
                        else go $ parse $ p s
                f@(Partial _) -> go $ feed f
                x             -> panic $ show x
        await >>= loop q

data StateP =  Outside  | Attring Text deriving Show

parseToken :: StateP -> Parser ( StateP, [ Token ] )
parseToken Outside = msum
    [ do
          t <- parseString ('<')
          guard $ B.length t > 0
          '<' <- peekChar'
          pure $ ( Outside, pure $ Ttext t )
    , do
          string "</"
          name <- takeWhile1 ((/=) '>')
          char '>'
          pure ( Outside, pure $ Tout $ T.strip $ toS name )
    , do
          () <$ char '<' <|> () <$ string "<?"
          skipSpace
          name <- T.strip . T.decodeUtf8 <$> takeWhile1
              (\x -> x /= ' ' && x /= '>')
          pure ( Attring name, pure $ Tin name )
    ]
parseToken (Attring name) = do
    skipSpace
    msum [ do
               () <$ char '>' <|> () <$ string "?>"
               pure ( Outside, pure $ TinC name )
         , do
               () <$ string "/>"
               pure ( Outside, [ TinC name, Tout name ] )
         , do
               attr <- takeWhile1 ((/=) '=')
               char '='
               skipSpace
               value <- parseQuoted
               pure ( Attring name
                    , pure $ Tattr (T.strip $ T.decodeUtf8 attr) value
                    )
         ]

escape :: Parser Char
escape = char '\\' >> '"' <$ satisfy (== '"')

amp :: Parser Char
amp = do
    char '&'
    x <- msum [ '<' <$ string "lt"
              , '>' <$ string "gt"
              , '&' <$ string "amp"
              , '"' <$ string "quot"
              , '\'' <$ string "apos"
              ]
    char ';'
    pure x

character :: Char -> Parser Char
character c = amp <|>  escape <|> satisfy (/= c)

parseString :: Char -> Parser ByteString
parseString c = B.pack <$> many (character c)

parseQuoted :: Parser ByteString
parseQuoted = do
    char '"'
    x <- parseString '"'
    char '"'
    pure x

