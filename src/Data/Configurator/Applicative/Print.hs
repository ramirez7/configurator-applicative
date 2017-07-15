{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
module Data.Configurator.Applicative.Print where

import           Data.List                     (intersperse)
import           Data.List.NonEmpty            (toList, NonEmpty)
import           Data.Maybe                    (mapMaybe)
import           Data.Monoid                   ((<>))
import           Data.SCargot.Print
import           Data.SCargot.Repr.WellFormed
import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           Data.Configurator.Applicative (Description (..), Lookup (..))

data DescTree a =
    Node a
  | OrNode [DescTree a]
  | AndNode [DescTree a]
  | ForkNode (DescTree a) (NonEmpty (a, DescTree a)) -- this type will probably change

-- TODO: Maybe we need a Pure node? Some way better than AndNode []

deriving instance Eq a => Eq (DescTree a)
deriving instance Show a => Show (DescTree a)

mkTree :: Description a -> DescTree Text
mkTree (NilD _)    = AndNode []
mkTree (LookupD lk) = Node $ prettyLookup lk
mkTree (OrD lhs rhs) = OrNode [mkTree lhs, mkTree rhs]
mkTree (AndD lhs rhs) = AndNode [mkTree lhs, mkTree rhs]
mkTree (ForkD descb options) = ForkNode (mkTree descb) $ fmap (\(x, d) -> (tshow x, mkTree d)) options

-- TODO: Make DescTree x where x is more structured than Text. SExpr module can handle printing it

simplify :: DescTree a -> DescTree a
simplify x@(Node _) = x
simplify (OrNode xs) = let s = split xs in OrNode $ AndNode (splitAnds s) : splitOrs s ++ splitOthers s
simplify (AndNode xs) = let s = split xs in AndNode $ OrNode (splitOrs s) : splitAnds s ++ splitOthers s
simplify x@(ForkNode _ _) = x

data DescSplit a = DescSplit
  { splitOrs    :: [DescTree a]
  , splitAnds   :: [DescTree a]
  , splitOthers :: [DescTree a]
  }

deriving instance Eq a => Eq (DescSplit a)
deriving instance Show a => Show (DescSplit a)

instance Monoid (DescSplit a) where
  mempty = DescSplit [] [] []
  (DescSplit a b c) `mappend` (DescSplit x y z) = DescSplit (a ++ x) (b ++ y) (c ++ z)

split :: [DescTree a] -> DescSplit a
split = foldMap $ \case
  x@(Node _) -> mempty { splitOthers = [x] }
  OrNode xs -> mempty { splitOrs = xs }
  AndNode xs -> mempty { splitAnds = xs }
  x@(ForkNode _ _) -> mempty { splitOthers = [x] }

filterTree :: DescTree a -> DescTree a
filterTree x@(Node _)       = x
filterTree (OrNode xs)      = OrNode (mapMaybe checkTree xs)
filterTree (AndNode xs)     = AndNode (mapMaybe checkTree xs)
filterTree x@(ForkNode _ _) = x

checkTree :: DescTree a -> Maybe (DescTree a)
checkTree x@(Node _)       = Just x
checkTree (OrNode [])      = Nothing
checkTree x@(OrNode _)     = Just x
checkTree (AndNode [])     = Nothing
checkTree x@(AndNode _)    = Just x
checkTree x@(ForkNode _ _) = Just x

toSExpr :: DescTree Text -> WellFormedSExpr Text
toSExpr (Node a)          = WFSAtom a
toSExpr (OrNode children) = WFSList $ "Or" : fmap toSExpr children
toSExpr (AndNode children) = WFSList $ "And" : fmap toSExpr children
toSExpr (ForkNode test forks) = WFSList $ "Fork" : toSExpr test : fmap forkSExpr (toList forks)
  where
    forkSExpr (cond, child) = [WFSAtom cond, toSExpr child]

prettySExp :: Description a -> Text
prettySExp = encodeOne (basicPrint id) . fromWellFormed . toSExpr . filterTree . simplify . mkTree

prettyKeys :: Description a -> Text
prettyKeys =  mconcat . intersperse "\n" . fmap prettySomeLookup . prettyLookupsList

data SomeLookup = forall a. SomeLookup (Lookup a)
prettySomeLookup (SomeLookup lk) = prettyLookup lk

prettyLookupsList :: Description a -> [SomeLookup]
prettyLookupsList (NilD _) = []
prettyLookupsList (LookupD lk) = [SomeLookup lk]
prettyLookupsList (OrD lhs rhs) = prettyLookupsList lhs ++ prettyLookupsList rhs
prettyLookupsList (AndD lhs rhs) = prettyLookupsList lhs ++ prettyLookupsList rhs
prettyLookupsList (ForkD descb options) = prettyLookupsList descb ++ (toList options >>= prettyLookupsList . snd)


prettyLookup :: Lookup a -> Text
prettyLookup Lookup{..} = lookupKey <> " :: " <> tshow lookupType <> " -- " <> lookupDescription

tshow :: Show a => a -> Text
tshow = T.pack . show
