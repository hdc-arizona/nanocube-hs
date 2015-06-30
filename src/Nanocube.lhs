> {-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}

> module Nanocube where

> import qualified Data.Map.Strict as Map
> import Data.Monoid
> import Text.JSON
> import Data.ByteString (ByteString)

The t type variable here should be either an Addr a t' or ()

--------------------------------------------------------------------------------

Structurally, a nanocube is either:

  - a list of data cubes (going down the different dimensions in which
we're building indices) and a rose tree representing a partition of
that dimension (where we're using strict maps for quick indexing and
updates of the rose tree, for reasons to be discussed below.)

  - or a single leaf summary

Summaries should be members of a commutative monoid; the assumption is
that we're building summaries of a sample from a distribution.  We're
assuming the samples are independent of each other as well, and so
there's a monoid homomorphism from the likelihood function of the
sample to the monoid holding the summaries. Because the likelihood
function of IID samples is a product of the likelihood function of the
individual draws, and products are commutative, the summaries should be
commutative as well.

In other words, the "s" type parameter should be a monoid.

In addition, t should be either a (Nanocube' s) or ().

> data Nanocube s a t
>    = Nanocube { nextDimension :: t,
>                 finer         :: Map.Map a (Nanocube s a t) }
>    | Leaf { summary :: s } 
>  deriving (Show, Eq)

--------------------------------------------------------------------------------

Nanocubes naturally form a monoid. (This implementation doesn't
actually require that $s$ be commutative in addition to being a
monoid; that comes from the desired homomorphism from the sample
space.)

> instance (Eq s, Ord a, Monoid s, Monoid t) => Monoid (Nanocube s a t) where

These are the trivial equations in order for nanocubes to be monoids.

>     mempty = mempty
>     mappend (Leaf s) x | s == mempty = x
>     mappend x (Leaf s) | s == mempty = x
>     mappend (Leaf s) (Leaf s') =
>         Leaf (s <> s')

Now we look at the equations to combine two non-leaf nanocubes. The
result will necessarily be a non-leaf nanocube, so we'll have to find
out what its nextDimension and finer fields are. nextDimension is
trivial by recursion: it's simply the monoid concatenation of the
respective nextDimension fields. The new refinement is essentially
the concatenation of the merged rose trees, where elements under
the same key are concatenated.

NOTE: This is not an efficient operation in terms of space usage of
the resulting nanocube. The recommended way to build a nanocube is
through the `add` function, rather than sending all individual samples
to the nanocube monoid and then concatenating them all
together. Consider these equations to be an alternative implementation
purely for the sake of clarity.

>     mappend (Nanocube next refinements) (Nanocube next' refinements') = 
>          let changes = map update (Map.assocs refinements)
>              update (k, c) = Map.alter (alteration c) k
>              alteration c Nothing   = Just c
>              alteration c' (Just c) = Just (c' <> c)
>              newRefinements = foldr ($) refinements' changes 
>           in Nanocube (next <> next') newRefinements 

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- > nextCube :: Nanocube s a t -> Maybe t
-- > nextCube (Nanocube c _) = Just c
-- > nextCube (Leaf _) = Nothing

-- > empty :: (Monoid s, Ord a) => Nanocube s a t
-- > empty = Leaf mempty

-- > class (CanAdd c a s t t') where
-- >   add :: (Eq s, Monoid s, Ord a) => Addr a t -> Nanocube s a t' -> Nanocube s a t'

-- > instance CanAdd (Nanocube s a ()) (Addr a ()) s () () where
-- >   add (MkAddr dimAddr ()) _ = error "foo"


-- > add :: (Eq s, Monoid s, Ord a) => Addr a -> s -> Nanocube s a -> Nanocube s a
-- > add (MkAddr [])            _ (Nanocube _ _) = error "non-uniform addressing?"
-- > add (MkAddr [])            s (Leaf s')  = Leaf (s <> s')
-- > add address@(MkAddr (_:_)) s l@(Leaf _) = add address s (Nanocube l Map.empty)
-- > add (MkAddr ((MkDimAddr []) : ads)) s (Nanocube next r) =
-- >     Nanocube (add (MkAddr ads) s next) r
-- > add (MkAddr ((MkDimAddr (l:ls)):ads)) s (Nanocube next r) =
-- >     let (newFinerCube@(Nanocube newFinerNext _), exists) = 
-- >            case Map.lookup l r of
-- >               Just c -> (add (MkAddr (MkDimAddr ls : ads)) s c, True)
-- >               Nothing -> (add (MkAddr (MkDimAddr ls : ads)) s empty, False)
-- >         newSingleton = Nanocube newFinerNext (Map.singleton l newFinerCube)
-- >      in case (Map.size r, exists) of
-- >            (0, False) -> newSingleton
-- >            (1, True) -> newSingleton
-- >            _ -> Nanocube (add (MkAddr ads) s next) $ Map.insert l newFinerCube r

-- > singleDimLookup :: (Eq s, Monoid s, Ord a) => DimAddr a -> Nanocube s a -> Nanocube s a
-- > singleDimLookup (MkDimAddr []) c = c
-- > singleDimLookup (MkDimAddr (_:_)) (Leaf _) = empty
-- > singleDimLookup (MkDimAddr (l:ls)) (Nanocube _ refinement) = 
-- >     maybe empty (singleDimLookup (MkDimAddr ls)) (Map.lookup l refinement)

-- > singleLookup :: (Eq s, Monoid s, Ord a) => Addr a -> Nanocube s a -> s
-- > singleLookup (MkAddr []) c = content c
-- > singleLookup (MkAddr (_:_)) (Leaf _) = error "Mismatched query?"
-- > singleLookup (MkAddr ((MkDimAddr []) : ads)) (Nanocube next _) =
-- >     singleLookup (MkAddr ads) next
-- > singleLookup (MkAddr ((MkDimAddr (l:ls)) : ads)) (Nanocube _ r) =
-- >     maybe mempty (singleLookup (MkAddr ((MkDimAddr ls) : ads))) (Map.lookup l r)

-- --------------------------------------------------------------------------------

-- > data QueryDim a = QuerySum [DimAddr a] | QuerySplit [DimAddr a]
-- > newtype Query a = MkQuery [QueryDim a]

-- > query :: (Monoid s, Eq s, Ord a) => Query a -> Nanocube s a -> Nanocube s [a]
-- > query (MkQuery []) c = Leaf $ content c
-- > query (MkQuery (QuerySum addrs : dims)) c =
-- >     let rs = map (\a -> query (MkQuery dims) $ nextCube $ singleDimLookup a c) addrs
-- >      in Nanocube (mconcat rs) Map.empty
-- > query (MkQuery (QuerySplit addrs : dims)) c = 
-- >     let rs = map (\a -> query (MkQuery dims) $ nextCube $ singleDimLookup a c) addrs
-- >         flatAddrs = map (\ (MkDimAddr a) -> a) addrs
-- >         nextMap = Map.fromList $ zip flatAddrs rs 
-- >      in Nanocube (mconcat rs) nextMap

-- > instance JSON a => JSON (Sum a) where
-- >   showJSON (Sum a) = showJSON a
-- >   readJSON = undefined

-- > asJSON :: (JSON s, JSON a) => Query a -> Nanocube s [a] -> JSValue
-- > asJSON (MkQuery []) c = showJSON $ content c
-- > asJSON (MkQuery (QuerySum _ : rest)) c = asJSON (MkQuery rest) c
-- > asJSON (MkQuery (QuerySplit _ : _)) (Leaf _) =
-- >     error "Internal error, shouldn't happen"
-- > asJSON (MkQuery (QuerySplit _ : rest)) (Nanocube _ r) =
-- >   JSArray (map makePair (Map.assocs r))
-- >  where
-- >   makePair (addrs, c) = 
-- >       showJSON (toJSObject [("key", showJSON addrs), 
-- >                             ("value", asJSON (MkQuery rest) c)])

-- > p1, p2, p3, p4 :: Addr Int
-- > p1 = MkAddr [MkDimAddr [1,2], MkDimAddr [1]]
-- > p2 = MkAddr [MkDimAddr [1,2], MkDimAddr [2]]
-- > p3 = MkAddr [MkDimAddr [1,1], MkDimAddr [1]]
-- > p4 = MkAddr [MkDimAddr [1,1], MkDimAddr [2]]

-- > c1, c2, c3, c4 :: Nanocube (Sum Int) Int
-- > c1 = add p1 (Sum 1) empty
-- > c2 = add p2 (Sum 1) c1
-- > c3 = add p3 (Sum 1) c2
-- > c4 = add p4 (Sum 1) c3

-- > class NCDim a where
-- >   dimAddr :: a -> DimAddr Int

-- > newtype NCSphericalMercator a = MkSM (a, a) deriving Show
-- > newtype NCLatLonDegs a = MkLLD (a, a) deriving Show
-- > newtype NCLatLon a = MkLL (a, a) deriving Show

-- > frac :: RealFrac a => a -> a
-- > frac x = x - (fromIntegral $ (floor x :: Integer))

-- > digits :: RealFrac a => a -> [Int]
-- > digits x' = digits' (frac x')
-- >   where digits' x | x >= 0.5 = 1 : (digits $ (x * 2))
-- >                   | otherwise = 0 : (digits $ (x * 2))

-- > quadTreeZip v1 v2 = zipWith (\x y -> x + 2 * y) (digits v1) (digits v2)

-- > instance (Floating a, RealFrac a) => NCDim (NCSphericalMercator a) where
-- >     dimAddr (MkSM (x, y)) = MkDimAddr $ quadTreeZip (x / (2 * pi) + 0.5) (y / (2 * pi) + 0.5)

-- > fromDegs :: (Floating a, RealFrac a) => NCLatLonDegs a -> NCLatLon a
-- > fromDegs (MkLLD (lat, lon)) = MkLL (lat * pi / 180.0, lon * pi / 180.0)

-- > fromLatLon :: (Floating a, RealFrac a) => NCLatLon a -> NCSphericalMercator a
-- > fromLatLon (MkLL (lat, lon)) = MkSM (lon, log $ tan $ (lat / 2 + pi / 4))

-- > truncateDim :: Int -> DimAddr a -> DimAddr a
-- > truncateDim n (MkDimAddr l) = MkDimAddr $ take n l

-- this requires a 2D dimension
-- regionCover :: (Floating a, RealFrac a, Integral b) => (a, a) -> (a, a) -> [DimAddr b]
-- regionCover 

--------------------------------------------------------------------------------

> data Addr a t = MkAddr (DimAddr a) t deriving Show
> newtype DimAddr a = MkDimAddr { getDimAddr :: [a] } deriving (Show, Eq)

> data NanoCons s r n = NanoCons { -- s_ummary, r_efinement, n_anocube
>   nanoTail :: n,
>   refine :: Map.Map r (NanoCons s r n)
>   }
> data NanoNil s = NanoNil { nilSummary :: s }

> class (HasContent c s) where
>   content :: c -> s
> instance HasContent (NanoNil s) s where
>   content (NanoNil s) = s
> instance (HasContent n s) => HasContent (NanoCons s r n) s where
>   content (NanoCons tail _) = content tail

Nanocubes are monoids, if you insist.

> instance (Eq s, Monoid s) => Monoid (NanoNil s) where
>   mempty = NanoNil mempty
>   mappend (NanoNil s) x | s == mempty = x
>   mappend x (NanoNil s) | s == mempty = x
>   mappend (NanoNil s) (NanoNil s') = NanoNil (s <> s')
> instance (Eq s, Monoid s, Ord r, Monoid n) => Monoid (NanoCons s r n) where
>   mempty = NanoCons mempty Map.empty
>   mappend (NanoCons c1 r1) (NanoCons c2 r2) =
>     let changes = map update (Map.assocs r1)
>         update (k, c) = Map.alter (alteration c) k
>         alteration c Nothing = Just c
>         alteration c' (Just c) = Just (c' <> c)
>         newRefinements = foldr ($) r2 changes
>      in NanoCons (c1 <> c2) newRefinements

... but you should really build them one point at a time via `add`, instead
of using the monoid interface

> class CanAdd s a n where
>   add :: a -> s -> n -> n
> instance (Eq s, Monoid s) => CanAdd s () (NanoNil s) where
>   add :: () -> s -> NanoNil s -> NanoNil s
>   add _ s (NanoNil s') = NanoNil (s' <> s)
> instance (Eq s, Monoid s, Ord r, CanAdd s a n, Monoid n) => CanAdd s (Addr r a) (NanoCons s r n) where
>   add :: Addr r a -> s -> NanoCons s r n -> NanoCons s r n
>   add address@(MkAddr (MkDimAddr (l:ls)) ads) s (NanoCons next r) =
>        let (newFinerCube@(NanoCons newFinerNext _), exists) =
>                  case Map.lookup l r of
>                      Just c -> (add (MkAddr (MkDimAddr ls) ads) s c, True)
>                      Nothing -> (add (MkAddr (MkDimAddr ls) ads) s mempty, False)
>            newSingleton = NanoCons newFinerNext (Map.singleton l newFinerCube)
>         in case (Map.size r, exists) of
>               (0, False) -> newSingleton
>               (1, True) -> newSingleton
>               _ -> NanoCons (add ads s next) $ Map.insert l newFinerCube r

> class SingleDimLookup a n where
>   singleDimLookup :: a -> n -> n
> instance (Eq s, Monoid s) => SingleDimLookup () (NanoNil s) where
>   singleDimLookup () n = n
> instance (Ord a, Eq s, Monoid s, Monoid n) => SingleDimLookup (DimAddr a) (NanoCons s a n) where
>   singleDimLookup :: (DimAddr a) -> NanoCons s a n -> NanoCons s a n
>   singleDimLookup (MkDimAddr []) c = c
>   singleDimLookup (MkDimAddr (l:ls)) (NanoCons _ refinement) =
>     maybe mempty (singleDimLookup (MkDimAddr ls)) (Map.lookup l refinement)

> class SingleLookup s a n where
>   singleLookup :: a -> n -> s
> instance (Eq s, Monoid s) => SingleLookup s () (NanoNil s) where
>   singleLookup () (NanoNil s) = s
> instance (Ord r,
>           Eq s, Monoid s, HasContent n s,
>           Monoid n,
>           SingleLookup s t n, SingleDimLookup r n) => SingleLookup s (Addr r t) (NanoCons s r n) where
>   singleLookup (MkAddr a rest) n =
>      singleLookup rest $ nanoTail $ singleDimLookup a n


> data QueryDim a = QuerySum [DimAddr a] | QuerySplit [DimAddr a]
> data Query a t = Query (QueryDim a) t

> class Query' q n n' where
>   query :: q -> n -> n'
> 
> instance Query' () (NanoNil s) (NanoNil s) where
>   query () s = s
> instance Query' t n n' => Query' (Query a t) (NanoCons s a n) (NanoCons s [a] n') where
>   query _ _ = error "Foo!"

--------------------------------------------------------------------------------

... why don't people do this?

> type a $ b = a b
> infixr 0 $

> p1 :: Addr Int $ Addr Int $ ()
> p1 = MkAddr (MkDimAddr [1 :: Int,2 :: Int]) $ MkAddr (MkDimAddr [1 :: Int]) $ ()
> p2 = MkAddr (MkDimAddr [1 :: Int,2 :: Int]) $ MkAddr (MkDimAddr [2 :: Int]) $ ()
> p3 = MkAddr (MkDimAddr [1 :: Int,1 :: Int]) $ MkAddr (MkDimAddr [1 :: Int]) $ ()
> p4 = MkAddr (MkDimAddr [1 :: Int,1 :: Int]) $ MkAddr (MkDimAddr [2 :: Int]) $ ()

> c0 :: NanoCons (Sum Int) Int $ NanoCons (Sum Int) Int $ NanoNil (Sum Int)

> c0 = mempty
> c1 = add p1 (Sum (1 :: Int)) c0
> c2 = add p2 (Sum (1 :: Int)) c1
> c3 = add p3 (Sum (1 :: Int)) c2
> c4 = add p4 (Sum (1 :: Int)) c3
