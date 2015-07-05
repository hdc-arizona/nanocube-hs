> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
> {-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}

> module Query where
>
> import Nanocube
> import Spatial
> import qualified Data.Map.Strict as Map
> import Data.Monoid
> import Text.JSON
> import Data.ByteString (ByteString)

A single lookup in a single dimension of a nanocube is simply a traversal
down the rose tree.

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

--------------------------------------------------------------------------------

> class Query' q n n' | q n -> n' where
>   query :: q -> n -> (q, n, n')

> instance Query' () (NanoNil s) (NanoNil s) where
>   query () s = ((), s, s)

--------------------------------------------------------------------------------
Simple queries aggregate multiple drill-downs independently; each DimAddr
traverses the nanocube dimension from the root.

> data SimpleQueryDim a = QuerySum [DimAddr a] | QuerySplit [DimAddr a]
> data SimpleQuery a t = SimpleQuery (SimpleQueryDim a) t

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

We return references to the original query and nanocube with the query
result because of a quirk on AsJSON, as described below.

> third :: (a,b,c) -> c
> third (a, b, c) = c
> 
> instance (Eq s, Monoid s, Ord a, Monoid n, Monoid n',
>           Singleton n' s, HasContent n' s,
>           Query' q n n') => Query' (SimpleQuery a q) (NanoCons s a n) (NanoCons s [a] n') where
>   query :: SimpleQuery a q -> NanoCons s a n -> (SimpleQuery a q, NanoCons s a n, NanoCons s [a] n')
>   query q@(SimpleQuery (QuerySum addrs) rest) c = 
>     let rs = map getSummary addrs
>         getSummary :: DimAddr a -> s
>         getSummary a = content $ third $ query rest $ nanoTail $ singleDimLookup a c
>      in (q, c, singleton $ mconcat rs)
>   query q@(SimpleQuery (QuerySplit addrs) rest) c =
>     let rs :: [NanoCons s [a] n']
>         rs = map getNextQuery addrs
>         getNextQuery :: DimAddr a -> NanoCons s [a] n'
>         getNextQuery a = NanoCons (third $ query rest $ nanoTail $ singleDimLookup a c) Map.empty
>         flatAddrs = map (\ (MkDimAddr a) -> a) addrs
>         nextMap :: Map.Map [a] (NanoCons s [a] n')
>         nextMap = Map.fromList $ zip flatAddrs rs
>         nextCube :: NanoCons s [a] n'
>         nextCube = mconcat rs
>      in (q, c, nextCube)

--------------------------------------------------------------------------------
Printing query results

It's silly that we have to satisfy the type checker by passing the
original nanocube here, but maybe that can be fixed later.

> class Query' q n n' => AsJSON q n n' | q n -> n' where
>   asJSON :: (q, n, n') -> JSValue
> instance JSON s => AsJSON () (NanoNil s) (NanoNil s) where
>   asJSON :: ((), NanoNil s, NanoNil s) -> JSValue
>   asJSON ((), _, NanoNil s) = showJSON s
> instance (Eq s, Ord s, Monoid s, JSON s,
>           Ord r, JSON r,
>           Monoid n, Monoid n',
>           AsJSON q' n n',
>           HasContent n' s, Singleton n' s) => AsJSON (SimpleQuery r q') (NanoCons s r n) (NanoCons s [r] n') where
>   asJSON :: (SimpleQuery r q', NanoCons s r n, NanoCons s [r] n') -> JSValue
>   asJSON (SimpleQuery (QuerySum _) rest, NanoCons next _, NanoCons rnext rrefine) = asJSON (rest, next, rnext)
>   asJSON (SimpleQuery (QuerySplit s) rest, NanoCons next _, NanoCons rnext rrefine) =
>     JSArray (map makePair (Map.assocs rrefine))
>       where
>         makePair (addrs, c) = showJSON (toJSObject [("key", showJSON addrs),
>                                                     ("value", asJSON (rest, next, rnext))])

