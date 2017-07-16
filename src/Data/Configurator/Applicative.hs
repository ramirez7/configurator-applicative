{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Data.Configurator.Applicative
 ( require
 , require'
 , fork
 , Parser()
 , keysFlat
 , keysNested
 --, sexp
 , printKeysFlat
 , printKeysNested
 --, printSexp
 --, runParserIO
 --, runParserEither
 , C.Configured(..)
 , C.Value(..)) where

import qualified Data.Configurator                    as C
import qualified Data.Configurator.Types              as C

import           Data.List.NonEmpty
import           Data.Text                            (Text)
import qualified Data.Text.IO                         as T
import           Data.Typeable

import           Data.Configurator.Applicative.Parser
import           Data.Configurator.Applicative.Print

require :: (C.Configured a, Typeable a) => Text -> Text -> Parser a
require key desc = require' key desc C.convert

require' :: forall a. Typeable a => Text -> Text -> (C.Value -> Maybe a) -> Parser a
require' key desc convert = LookupP (Lookup key (typeRep (Proxy :: Proxy a)) (unempty desc) convert)
  where
    unempty "" = Nothing
    unempty s  = Just s

-- re-export Parser (no constructors)
-- applicative instance
-- alternative instance

fork :: (Eq b, Show b) => Parser b -> NonEmpty (b, Parser a) -> Parser a
fork = ForkP

printKeysFlat :: Parser a -> IO ()
printKeysFlat = T.putStrLn . keysFlat

printKeysNested :: Parser a -> IO ()
printKeysNested = T.putStrLn . keysNested
-- keyList :: Parser a -> Text -- newlines and all
-- keyMap :: Parser a -> Text -- newlines and all
-- sexp :: Parser a -> Text -- newlines and all

-- runParserIO/Either

-- re-export some Configurator stuff
