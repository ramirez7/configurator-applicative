{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}

module Data.Configurator.Applicative.Parser where

import           Control.Applicative
import           Control.Monad.Except    (MonadError (..))
import           Control.Monad.IO.Class  (MonadIO (..))
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.Foldable           as F
import           Data.List.NonEmpty
import           Data.Proxy              (Proxy (..))
import           Data.Text               (Text)
import           Data.Typeable           (TypeRep, Typeable, typeRep)

-- Lookup
data Lookup a = Lookup
  { lookupKey         :: Text
  , lookupType        :: TypeRep
  , lookupDescription :: Maybe Text
  , lookupFunc        :: C.Value -> Maybe a
  } deriving (Functor)

doLookup :: Lookup a -> C.Config -> IO (Maybe a)
doLookup lk cfg = (>>= lookupFunc lk) <$> C.lookup cfg (lookupKey lk)

-- Parsing
data Parser a where
  NilP :: Maybe a -> Parser a
  LookupP :: Lookup a -> Parser a
  OrP :: Parser a -> Parser a -> Parser a
  AndP :: Parser (a -> b) -> Parser a -> Parser b
  ForkP :: forall a b. (Eq b, Show b) => Parser b -> NonEmpty (b, Parser a) -> Parser a

instance Functor Parser where
  fmap f (NilP ma)       = NilP (f <$> ma)
  fmap f (LookupP lk)    = LookupP (fmap f lk)
  fmap f (OrP x y)       = OrP (fmap f x) (fmap f y)
  fmap f (AndP dfab da)  = AndP (fmap (f.) dfab) da
  fmap f (ForkP db opts) = ForkP db (fmap (\(b, da) -> (b, f <$> da)) opts)

instance Applicative Parser where
  pure = NilP . Just
  (<*>) = AndP

instance Alternative Parser where
  empty = NilP Nothing
  (<|>) = OrP

parse :: (MonadError () m, MonadIO m) => Parser a -> C.Config -> m a
parse (NilP ma) _ = ma `orThrowError` () -- fail. Need to add context arg to 'parse' probably?
parse (LookupP lk) cfg = liftIO (doLookup lk cfg) `orThrowErrorM` () -- failed lookup (reason not visible)
parse (OrP lhs rhs) cfg = parse lhs cfg `catchError` const (parse rhs cfg)
parse (AndP lhs rhs) cfg = parse lhs cfg <*> parse rhs cfg
parse (ForkP descb options) cfg = do
  b <- parse descb cfg
  (_, desca) <- F.find (\(x, _) -> x == b) options `orThrowError` () -- no matching route
  parse desca cfg

-- Helpers
orThrowError :: (MonadError e m) => Maybe a -> e -> m a
orThrowError (Just a) _ = return a
orThrowError Nothing e  = throwError e

orThrowErrorM :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrowErrorM mma e = mma >>= (`orThrowError` e)
