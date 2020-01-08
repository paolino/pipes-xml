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

module Pipes.XML where

import           Control.Arrow                  ( (***) )
import           Control.Lens            hiding ( deep
                                                , none
                                                )
import           Control.Lens.Extras
import Control.Monad.Cont
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


-- | node attributes
type Attrs = Map ByteString ByteString

-- consume all attrs of a node
getAttrs :: Functor m => Pipe E a m Attrs
getAttrs = go mempty
  where
    go m = do
        x <- await
        case x of
            Tattr k v -> go (M.insert k v $ m)
            TinC _        -> pure m
            x -> panic $ "unterminated node attrs: " <>  show x

type Inside m a = Attrs -> Pipe E a m ()
-- | euler scanner, suspend after a tag opening named as the first argument
-- the second argument is a Pipe to receive the tag internal tokens
-- TODO check out relative depth is 0 before decide the Tout is correct for bail out
findTag
    :: (MonadIO m, Functor m)
    => ByteString
    -> Inside m a
    -> Pipe E a m ()
findTag t inside = do
    x <- await
    case x of
        Tin c -> case c == t of
            True -> do
                m <- getAttrs
                (>->)
                    do void $ breakP (\e -> e ^? _Tout == Just c) 
                    do forever $ inside m -- >> forever await
            False -> findTag t inside
        _ -> findTag t inside

findTag_ t inside = findTag t $ const inside

newtype CPipe a b m x = CPipe (ContT () (Pipe a b m) x) deriving (Monad, Applicative, Functor)

runCPipe :: Functor m => CPipe a b m () -> Pipe a b m ()
runCPipe (CPipe f) = runContT f return

liftCP :: Pipe a b m x -> CPipe a b m x
liftCP = CPipe . lift

findTagC :: MonadIO m =>  ByteString -> CPipe E a m Attrs
findTagC b = CPipe $ ContT $ findTag b

instance (Functor m, Semigroup x) => Semigroup (CPipe a b m x) where
    CPipe (ContT f) <> CPipe (ContT g) = CPipe $ ContT $ \c -> (<>) <$> f c <*> g c 

instance (Functor m, Monoid x) => Monoid (CPipe a b m x) where
    mempty = pure mempty

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

{-
getVMs :: forall m . MonadIO m => Pipe E OVM m ()
getVMs  = runCPipe $ do 
        findTagC "VmRestorePoints" 
        findTagC "VmRestorePoint"
        mappend
            do
                liftCP $ yield RP 
                findTagC "Links"
                m <- findTagC "Link" 
                let mr =  do 
                        "BackupFileReference" <- m ^? ix "Type"
                        m ^? ix "Name" 
                case mr of 
                    Nothing -> pure () 
                    Just x -> liftCP $ yield (RPT $ isLTFile x)
                liftCP $ forever await
            do
                findTagC "HierarchyObjRef"
                liftCP $ getText (parseVM . toS) //> yield . VM
-}