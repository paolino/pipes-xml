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

module Pipes.XML where

import           Control.Arrow                  ( (***) )
import           Control.Lens            hiding ( deep
                                                , none
                                                )
import           Control.Lens.Extras
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

import           Protolude               hiding ( to
                                                , lefts
                                                , rights
                                                , second
                                                , first
                                                )
-- import Persist
import           Pipes
import           Pipes.Core
import qualified Pipes.Prelude                 as P
import           Pipes.Safe              hiding ( handle )

import qualified Xeno.SAX                      as X
import qualified Xeno.DOM                      as X

import           Prelude                        ( String
                                                , lookup
                                                , tail
                                                )
import qualified Pipes.Lift                    as PL

-- | tokens
data E 
    = Tin ByteString
    | TinC ByteString 
    | Tattr ByteString ByteString 
    | Tout ByteString 
    | Ttext ByteString 
    | Tcdata ByteString
    deriving Show

makePrisms ''E




produceE :: Functor m 
    => ByteString -- ^ xml, strict because we use Xeno as tokenizer
    -> Producer E m ()
produceE = X.process
    do yield . Tin
    do \n v -> yield $ Tattr n v
    do yield . TinC
    do yield . Ttext
    do yield . Tout
    do yield . Tcdata


-- a pipe which breaks, and return the breaking token
breakP :: Functor m => (a -> Bool) -> Pipe a a m a
breakP f = do
    x <- await
    if f x then pure x else yield x >> breakP f



type Attrs = Map ByteString ByteString

getAttrs :: Functor m => Pipe E a m Attrs
getAttrs = go mempty
  where
    go m = do
        x <- await
        case x of
            Tattr k v -> go (M.insert k v $ m)
            _         -> pure m

findTag
    :: (MonadIO m, Functor m)
    => ByteString
    -> Pipe E a m (Attrs, Pipe E a m () -> Pipe E a m ())
findTag t = do
    x <- await
    case x of
        Tin c -> case c == t of
            True -> do
                m <- getAttrs
                pure
                    ( m
                    , \p -> (breakP (\e -> e ^? _Tout == Just c) >-> forever p)
                        >> void await
                    )
            False -> findTag t
        _ -> findTag t

getText :: Functor m => (ByteString -> Maybe a) -> Pipe E a m ()
getText f = do
    t <- await
    case t of
        Ttext c -> do
            case f c of
                Just x  -> yield x
                Nothing -> pure ()
            getText f
        _ -> getText f


foldUntilP
    :: Functor m => Monoid a => (b -> Bool) -> (b -> a) -> Pipe b (b, a) m ()
foldUntilP f g = go mempty
  where
    go y = do
        x <- await
        case f x of
            False -> yield (x, y)
            True  -> go $ y <> g x
