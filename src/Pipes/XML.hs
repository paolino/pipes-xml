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
import           Data.Map                       ( Map, insert )
import           Protolude               
import           Pipes
import           Pipes.Core
import           Pipes.Safe              hiding ( handle )

import Pipes.XML.Token

-- | a pipe which breaks, and return the breaking token
--
breakP :: Functor m => (a -> Bool) -> Pipe a a m a
breakP f = do
    x <- await 
    if f x then pure x else yield x >> breakP f

-- | node attributes
type Attrs = Map Text ByteString

-- | consume all attrs of a node
getAttrs :: Functor m => Pipe Token a m Attrs
getAttrs = go mempty 
  where
    go m = do
        x <- await
        case x of
            Tattr k v -> go $ insert k v $ m
            TinC _    -> pure m
            x         -> panic $ "unterminated node attrs: " <> show x

-- | condition of pipe repetition
data Loop = Loop | Stop 

instance Semigroup Loop where  
    Loop <> Loop = Loop
    _ <> _ = Stop

instance Monoid Loop where
    mempty = Loop

-- | euler scanner, suspend after a tag opening named as the first argument
-- the second argument is a Pipe to receive the tag internal tokens
-- TODO check out relative depth is 0 before decide the Tout is correct for bail out
insideTag
    :: Functor m
    => Text
    -> (Attrs -> Pipe Token a m Loop)
    -> Pipe Token a m Loop
insideTag t inside = do
    x <- await
    case x of
        Tin c -> case c == t of
            True -> do
                m <- getAttrs
                (>->)
                    do  breakP (\x -> x ^? _Tout == Just c) >> pure mempty
                    do  let r Loop   = inside m >>= r 
                            r Stop   = forever await
                        r Loop
                        
            False -> insideTag t inside
        _ -> insideTag t inside


-- | dsl to flatten the nested pipe structure reflecting xml structure
newtype CPipe a b m x = CPipe (ContT Loop (Pipe a b m) x)
    deriving ( Monad, Applicative, Functor, MonadCont )

-- | interpret the resulting pipe
renderPipe :: Functor m => CPipe a b m Loop -> Pipe a b m Loop
renderPipe (CPipe f) = runContT f return

-- | promote a pipe operation
pipe :: Functor m => Pipe a b m x -> CPipe a b m x
pipe = CPipe . lift

instance MonadTrans (CPipe a b ) where
    lift = pipe . lift 

-- | declare non repetition of the run pipe
stop :: Functor m => CPipe a b m Loop
stop = pure Stop

-- | declare repetition of the run pipe
loop :: Functor m => CPipe a b m Loop
loop = mempty

-- | consume some sibilings
takeP :: Functor m => Int -> CPipe a b m Loop -> CPipe a b m Loop
takeP n = fold . replicate n

-- | consume next tag matching
tag :: Functor m =>  Text -> CPipe Token a m Attrs
tag b = CPipe $ ContT $ \m -> insideTag b m >> pure Stop

-- | consume all tags matching
tags :: Functor m =>  Text -> CPipe Token a m Attrs
tags b = CPipe $ ContT $ \m -> insideTag b m >> pure Loop

instance (Functor m, Semigroup x) => Semigroup (CPipe a b m x) where
    CPipe (ContT f) <> CPipe (ContT g) = CPipe $ ContT $ \c -> (<>) <$> f c <*> g c 

instance (Functor m, Monoid x) => Monoid (CPipe a b m x) where
    mempty = pure mempty

-- | try to consume and parse next text
getText :: Functor m => (ByteString -> Maybe a) -> Pipe Token a m ()
getText f = do
    let p c = case f c of
            Just x  -> yield x
            Nothing -> pure ()
    t <- await
    case t of
        Ttext c -> p c 
        -- Tcdata c -> p c 
        _ -> getText f
    
produceTokens :: Functor m => Pipe ByteString Token m ()
produceTokens =  chunking Outside parseToken 
 

