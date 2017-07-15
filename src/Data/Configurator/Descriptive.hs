{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}

module Data.Configurator.Descriptive where

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
  , lookupDescription :: Text
  , lookupFunc        :: C.Value -> Maybe a
  } deriving (Functor)

doLookup :: Lookup a -> C.Config -> IO (Maybe a)
doLookup lk cfg = (>>= lookupFunc lk) <$> C.lookup cfg (lookupKey lk)

-- Parsing
data Description a where
  NilD :: Maybe a -> Description a
  LookupD :: Lookup a -> Description a
  OrD :: Description a -> Description a -> Description a
  AndD :: Description (a -> b) -> Description a -> Description b
  ForkD :: forall a b. (Eq b, Show b) => Description b -> NonEmpty (b, Description a) -> Description a

instance Functor Description where
  fmap f (NilD ma)       = NilD (f <$> ma)
  fmap f (LookupD lk)    = LookupD (fmap f lk)
  fmap f (OrD x y)       = OrD (fmap f x) (fmap f y)
  fmap f (AndD dfab da)  = AndD (fmap (f.) dfab) da
  fmap f (ForkD db opts) = ForkD db (fmap (\(b, da) -> (b, f <$> da)) opts)

instance Applicative Description where
  pure = NilD . Just
  (<*>) = AndD

instance Alternative Description where
  empty = NilD Nothing
  (<|>) = OrD

parse :: (MonadError () m, MonadIO m) => Description a -> C.Config -> m a
parse (NilD ma) _ = ma `orThrowError` () -- fail
parse (LookupD lk) cfg = liftIO (doLookup lk cfg) `orThrowErrorM` () -- failed lookup (reason not visible)
parse (OrD lhs rhs) cfg = parse lhs cfg `catchError` const (parse rhs cfg)
parse (AndD lhs rhs) cfg = parse lhs cfg <*> parse rhs cfg
parse (ForkD descb options) cfg = do
  b <- parse descb cfg
  (_, desca) <- F.find (\(x, _) -> x == b) options `orThrowError` () -- no matching route
  parse desca cfg

-- Helpers
orThrowError :: (MonadError e m) => Maybe a -> e -> m a
orThrowError (Just a) _ = return a
orThrowError Nothing e  = throwError e

orThrowErrorM :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrowErrorM mma e = mma >>= (`orThrowError` e)

-- Scratch
-- | TODO: This will be somewhere in the API
lookup' :: forall a. (C.Configured a, Typeable a) => Text -> Text -> Description a
lookup' key desc = LookupD (Lookup key (typeRep (Proxy :: Proxy a)) desc C.convert)

data Person = Person
  { firstName :: Text
  , lastName  :: Text
  , age       :: Int
  } deriving (Eq, Show, Typeable)

personDescription :: Description Person
personDescription =
  Person <$> lookup' "person.first" "their first name"
         <*> lookup' "person.last" "their last name"
         <*> lookup' "person.age" "their age"

data DatabaseConfig = RDBMS { user :: Text, password :: Text } | File { path :: Text } | InMemory deriving (Eq, Show, Typeable)
databaseConfigDescription :: Description DatabaseConfig
databaseConfigDescription = ForkD (lookup' "database.type" "") $
  [ ("rdbms" :: Text, RDBMS <$> lookup' "database.user" "" <*> lookup' "database.password" "")
  , ("file", File <$> lookup' "database.path" "")
  , ("in-memory", pure InMemory)
  ]
