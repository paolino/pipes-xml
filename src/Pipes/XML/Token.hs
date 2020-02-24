{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Pipes.XML.Token where

import           Control.Arrow                    ((***))
import           Control.Lens                     hiding (deep, none)
import           Control.Lens.Extras
import           Control.Monad.Cont
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Builder          as B
import qualified Data.ByteString.Char8            as B
import           Data.Map                         (Map, insert)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Pipes
import           Pipes.Core
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       hiding (handle)
import           Protolude


-- | xml tokens
data Token = Tin Text -- ^ tag starts
           | TinC Text -- ^  end of attribute list
           | Tattr Text ByteString -- ^ an atttribute
           | Tout Text -- ^ tag ends
           | Ttext ByteString -- ^ some text
           | Tcdata ByteString -- ^ some CDATA -- not implemented
           | Tcomment Text
           | Tpreproc Text ByteString
           | Tdtd Text (Either ByteString [ByteString])
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

data StateP =  Outside  | Attring Text | Elements Text deriving Show

dtdElements :: Parser [ByteString]
dtdElements = do
    char '['
    xs <- many dtdElement
    skipSpace
    char ']' 
    pure xs

dtdElement :: Parser ByteString 
dtdElement = do
    skipSpace
    string "<!"
    skipSpace
    parseString '>' <* char '>'

-- ts = traceShow

parseToken :: StateP -> Parser ( StateP, [ Token ] )
parseToken Outside = msum
    [  do
          string "</"
          name <- parseString '>' <* char '>'
          pure ( Outside, pure $ Tout $ T.strip $ toS name )
    , do
          string "<!--"
          skipSpace
          c <- T.decodeUtf8 <$> parseStringS "-->" 
          pure ( Outside, pure $ Tcomment c)
    , do
          string "<?"
          skipSpace
          n <- T.decodeUtf8 <$> parseString ' '
          skipSpace
          c <- parseStringS "?>" 
          pure ( Outside, pure $ Tpreproc n c)
    , do
          string "<!"
          skipSpace
          string "DOCTYPE"
          skipSpace
          n <- T.decodeUtf8 <$> parseString ' '
          skipSpace
          xs <- Right <$> dtdElements -- <|>  Left <$> parseQuoted
          skipSpace 
          char '>'
          pure (Outside, pure $ Tdtd n xs)

    , do
          char '<' 
          skipSpace
          (n, d) <- parseStringP (0 <$ char ' ' <|> 1 <$ char  '>' <|> 2 <$ string "/>")
          let name = T.decodeUtf8 n
          case d of
              0 -> pure ( Attring name, pure $ Tin name )
              1 -> pure ( Outside, [Tin name, TinC name] )
              2 -> pure ( Outside, [Tin name, TinC name, Tout name])
    , do
          t <- parseString '<'
          guard $ B.length t > 0
          '<' <- peekChar'
          pure (Outside, pure $ Ttext t)
    
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
               attr <- takeWhile1 ('=' /=)
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

characterS :: Parser a -> Parser ([Char],a)
characterS c = (mempty,) <$> c 
    <|>  first . (:) <$>  (amp <|>  escape <|>  anyChar) <*> characterS c

parseString :: Char -> Parser ByteString
parseString c = B.pack <$> many (character c)

parseStringS :: ByteString -> Parser ByteString 
parseStringS c = fst  <$> parseStringP (string c)

parseStringP :: Parser a -> Parser (ByteString, a)
parseStringP c = first B.pack  <$> characterS c

parseQuoted :: Parser ByteString
parseQuoted = do
    char '"'
    x <- parseString '"'
    char '"'
    pure x

