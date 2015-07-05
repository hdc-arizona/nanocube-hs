> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
> {-# LANGUAGE UndecidableInstances, FlexibleInstances, TypeSynonymInstances #-}
> {-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}

> module Query where
>
> import Nanocube
> import Spatial
> import qualified Data.Map.Strict as Map
> import Data.Monoid
> import Data.Maybe
> import Text.JSON
> import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
A single lookup in a single dimension of a nanocube is simply a traversal
down the rose tree.

Single-dimension, single-cell lookup (mostly a warmup for what's next)

> class SingleDimLookup a n where
>   singleDimLookup :: a -> n -> n
> instance (Eq s, Monoid s) =>
>     SingleDimLookup ()          (NanoNil s) where
>   singleDimLookup :: () -> (NanoNil s) -> NanoNil s
>   singleDimLookup () n = n
> instance (Ord a, Eq s, Monoid s, Monoid n) =>
>     SingleDimLookup (DimAddr a) (NanoCons s a n) where
>   singleDimLookup :: (DimAddr a) -> NanoCons s a n -> NanoCons s a n
>   singleDimLookup (MkDimAddr []) c = c
>   singleDimLookup (MkDimAddr (l:ls)) (NanoCons _ refinement) =
>     maybe mempty (singleDimLookup (MkDimAddr ls)) (Map.lookup l refinement)

Single-dimension, single-cell lookup, returning the monoid

> class SingleLookup s a n | a n -> s where
>   singleLookup :: a -> n -> s
> instance (Eq s, Monoid s) => SingleLookup s () (NanoNil s) where
>   singleLookup () (NanoNil s) = s
> instance (Ord r,
>           Eq s, Monoid s, HasContent n s,
>           Monoid n,
>           SingleLookup s t n, SingleDimLookup r n) => SingleLookup s (Addr r t) (NanoCons s r n) where
>   singleLookup (MkAddr a rest) n =
>      singleLookup rest $ nanoTail $ singleDimLookup a n

--------------------------------------------------------------------------------

Querying

We return references to the original query and nanocube with the query
result because of a quirk on AsJSON, as described below.

> class Query' q n n' | q n -> n' where
>   query :: q -> n -> (q, n, n')
> instance Query' () (NanoNil s) (NanoNil s) where
>   query () s = ((), s, s)

> data QueryT = QuerySum | QuerySplit
> data Query a t = Query QueryT a t

Printing query results

It's silly that we have to satisfy the type checker by passing the
original nanocube here, but maybe that can be fixed later.

> class Query' q n n' => AsJSON q n n' | q n -> n' where
>   asJSON :: (q, n, n') -> JSValue
> instance JSON s => AsJSON () (NanoNil s) (NanoNil s) where
>   asJSON ((), _, NanoNil s) = showJSON s

--------------------------------------------------------------------------------
Simple queries aggregate multiple drill-downs independently; each DimAddr
traverses the nanocube dimension from the root.

> newtype SimpleQueryDim a = MkSimpleQueryDim [DimAddr a]
> type SimpleQuery a q = Query (SimpleQueryDim a) q
> 
> third :: (a,b,c) -> c
> third (a, b, c) = c
>
> instance (Eq s, Monoid s, Ord a, Monoid n, Monoid n',
>           Singleton n' s, HasContent n' s,
>           Query' q n n') => Query' (SimpleQuery a q) (NanoCons s a n) (NanoCons s [a] n') where
>   query :: SimpleQuery a q -> NanoCons s a n -> (SimpleQuery a q, NanoCons s a n, NanoCons s [a] n')
>   query q@(Query QuerySum (MkSimpleQueryDim addrs) rest) c = 
>     let rs = map (content . getNextQuery) addrs
>         getNextQuery :: DimAddr a -> NanoCons s [a] n'
>         getNextQuery a = NanoCons (third $ query rest $ nanoTail $ singleDimLookup a c) Map.empty
>      in (q, c, singleton $ mconcat rs)
>   query q@(Query QuerySplit (MkSimpleQueryDim addrs) rest) c =
>     let rs = map getNextQuery addrs
>         getNextQuery :: DimAddr a -> NanoCons s [a] n'
>         getNextQuery a = NanoCons (third $ query rest $ nanoTail $ singleDimLookup a c) Map.empty
>         flatAddrs = map (\ (MkDimAddr a) -> a) addrs
>         nextMap :: Map.Map [a] (NanoCons s [a] n')
>         nextMap = Map.fromList $ zip flatAddrs rs
>         nextCube :: NanoCons s [a] n'
>         nextCube = mconcat rs
>      in (q, c, nextCube)

> instance (Eq s, Ord s, Monoid s, JSON s,
>           Ord r, JSON r,
>           Monoid n, Monoid n',
>           AsJSON q' n n',
>           HasContent n' s, Singleton n' s) => AsJSON (SimpleQuery r q') (NanoCons s r n) (NanoCons s [r] n') where
>   asJSON :: (SimpleQuery r q', NanoCons s r n, NanoCons s [r] n') -> JSValue
>   asJSON (Query QuerySum (MkSimpleQueryDim _) rest, NanoCons next _, NanoCons rnext rrefine) =
>     showJSON (toJSObject [("sum", asJSON (rest, next, rnext))])
>   asJSON (Query QuerySplit (MkSimpleQueryDim s) rest, NanoCons next _, NanoCons rnext rrefine) =
>     JSArray (map makePair (Map.assocs rrefine))
>       where
>         makePair (addrs, c) = showJSON (toJSObject [("split", toJSObject [("key", showJSON addrs),
>                                                                           ("value", asJSON (rest, next, rnext))])])

--------------------------------------------------------------------------------
RegionQueries collect the single-dimension drill-downs in one pass,
and so are slightly more efficient during traversal.

> type RegionQuery a q = Query (Region a) q
>
> instance (Eq s, Monoid s, Ord a, Monoid n, Monoid n',
>           Singleton n' s, HasContent n' s,
>           Query' q n n') => Query' (RegionQuery a q) (NanoCons s a n) (NanoCons s a n') where
>   query :: RegionQuery a q -> NanoCons s a n -> (RegionQuery a q, NanoCons s a n, NanoCons s a n')
>   query q@(Query QuerySum r rest) c = 
>     let getSummary = third . query rest . nanoTail
>      in (q, c, NanoCons (foldMap getSummary $ visitRegion c r) Map.empty)
>   query q@(Query QuerySplit r rest) c =
>     let getNextQuery :: Region a -> NanoCons s a n -> NanoCons s a n'
>         getNextQuery (MkRegion (Left ())) c = NanoCons (third $ query rest $ nanoTail c) Map.empty
>         getNextQuery (MkRegion (Right m)) c = NanoCons (singleton (mempty :: s)) nextRefine
>           where
>             nextCube (k, r) = do refineC <- Map.lookup k (refine c)
>                                  let resultC = getNextQuery r refineC
>                                  return (k, resultC)
>             nextRefine = Map.fromList $ catMaybes $ map nextCube $ Map.assocs m
>      in (q, c, getNextQuery r c)

> instance (Eq s, Ord s, Monoid s, JSON s,
>           Ord r, JSON r, Show r,
>           Monoid n, Monoid n',
>           AsJSON q' n n',
>           HasContent n' s, Singleton n' s) => AsJSON (RegionQuery r q') (NanoCons s r n) (NanoCons s r n') where
>   asJSON :: (RegionQuery r q', NanoCons s r n, NanoCons s r n') -> JSValue
>   asJSON (Query QuerySum region rest, (NanoCons next _), (NanoCons rnext _)) =
>     showJSON (toJSObject [("sum", asJSON (rest, next, rnext))])
>   asJSON (Query QuerySplit region rest, (NanoCons next _), c) = 
>     showJSON (toJSObject [("split", f c)])
>       where
>         f :: NanoCons s r n' -> JSValue
>         f (NanoCons rnext m) | Map.size m == 0 = showJSON $ toJSObject [("value", asJSON (rest, next, rnext))]
>                              | otherwise       = showJSON $ toJSObject $ Prelude.map (\ (k, v) -> (show k, f v)) (Map.assocs m)
