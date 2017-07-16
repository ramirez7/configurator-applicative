{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
module Data.Configurator.Applicative.Print where

import qualified Data.Foldable                        as F
import           Data.List                            (intersperse)
import           Data.List.NonEmpty                   (NonEmpty, toList)
import           Data.Map                             (Map)
import qualified Data.Map                             as M
import           Data.Maybe                           (mapMaybe)
import           Data.Monoid                          ((<>))
import           Data.SCargot.Print
import           Data.SCargot.Repr.WellFormed
import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Data.Configurator.Applicative.Parser (Lookup (..), Parser (..))

data DescTree a =
    Node a
  | OrNode [DescTree a]
  | AndNode [DescTree a]
  | ForkNode (DescTree a) (NonEmpty (a, DescTree a)) -- this type will probably change

-- TODO: Maybe we need a Pure node? Some way better than AndNode []

deriving instance Eq a => Eq (DescTree a)
deriving instance Show a => Show (DescTree a)

mkTree :: Parser a -> DescTree Text
mkTree (NilP _)    = AndNode []
mkTree (LookupP lk) = Node $ prettyLookup lk
mkTree (OrP lhs rhs) = OrNode [mkTree lhs, mkTree rhs]
mkTree (AndP lhs rhs) = AndNode [mkTree lhs, mkTree rhs]
mkTree (ForkP descb options) = ForkNode (mkTree descb) $ fmap (\(x, d) -> (tshow x, mkTree d)) options

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

prettySExp :: Parser a -> Text
prettySExp = encodeOne (basicPrint id) . fromWellFormed . toSExpr . filterTree . simplify . mkTree

keysFlat :: Parser a -> Text
keysFlat =  mconcat . intersperse "\n" . fmap prettySomeLookup . lookupsList

keysNested :: Parser a -> Text
keysNested = mconcat . intersperse "\n" . go 0 . lookupsMap
  where
    go ident (LookupMap mp sls) =
          M.foldMapWithKey (\k lm -> mkIdent ident <> k : go (ident + 2) lm) mp
          ++ fmap (\sl -> mkIdent ident <> prettySomeLookup sl) sls
    mkIdent = flip T.replicate " "

data SomeLookup = forall a. SomeLookup (Lookup a)

prettySomeLookup :: SomeLookup -> Text
prettySomeLookup (SomeLookup lk) = prettyLookup lk

lookupsList :: Parser a -> [SomeLookup]
lookupsList (NilP _) = []
lookupsList (LookupP lk) = [SomeLookup lk]
lookupsList (OrP lhs rhs) = lookupsList lhs ++ lookupsList rhs
lookupsList (AndP lhs rhs) = lookupsList lhs ++ lookupsList rhs
lookupsList (ForkP descb options) = lookupsList descb ++ (toList options >>= lookupsList . snd)

data LookupMap = LookupMap (Map Text LookupMap) [SomeLookup]

lookupsMap :: Parser a -> LookupMap
lookupsMap (NilP _) = LookupMap mempty mempty
lookupsMap (LookupP lk) = mkLookupMap lk
lookupsMap (OrP lhs rhs) = unionLookupMap (lookupsMap lhs) (lookupsMap rhs)
lookupsMap (AndP lhs rhs) = unionLookupMap (lookupsMap lhs) (lookupsMap rhs)
lookupsMap (ForkP descb options) = unionLookupMap (lookupsMap descb) (F.foldl' unionLookupMap (LookupMap mempty mempty) $ fmap (lookupsMap . snd) options)

mkLookupMap :: Lookup a -> LookupMap
mkLookupMap lk = go (T.splitOn "." (lookupKey lk))
  where
    go []       = error "impossible due to splitOn"
    go [x]      = LookupMap mempty $ pure (SomeLookup $ lk { lookupKey = x })
    go (x : xs) = LookupMap (M.singleton x (go xs)) mempty

unionLookupMap :: LookupMap -> LookupMap -> LookupMap
unionLookupMap (LookupMap xmp xls) (LookupMap ymp yls)= LookupMap (M.unionWith unionLookupMap xmp ymp) (xls ++ yls)

prettyLookup :: Lookup a -> Text
prettyLookup Lookup{..} = lookupKey <> " :: " <> tshow lookupType <> maybe "" (" -- " <>) lookupDescription

tshow :: Show a => a -> Text
tshow = T.pack . show
