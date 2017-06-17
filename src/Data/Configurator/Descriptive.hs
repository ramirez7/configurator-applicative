{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Configurator.Descriptive where

import Data.List.NonEmpty

data Description a where
  Val :: a -> Description a
  Or :: Description a -> Description a -> Description a
  Fork :: forall a b. NonEmpty (Description b, Description a) -> Description a
