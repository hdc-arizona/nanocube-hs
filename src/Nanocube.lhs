> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
> {-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}
>
> module Nanocube where
>
> import qualified Data.Map.Strict as Map
> import Data.Monoid
> import Text.JSON
> import Data.ByteString (ByteString)

The t type variable here should be either an Addr a t' or ()

--------------------------------------------------------------------------------

Structurally, a nanocube is either:

  - a list of data cubes (going down the different dimensions in which
we're building indices) and a rose tree representing a hierarchical
partition of that dimension (where we're using strict maps for quick
indexing and updates of the rose tree, for reasons to be discussed
below.)
  - or a single leaf summary

Maybe in the future we can add implementation specific datatypes
(categorical dimensions, for example, might have different
implementations instead of the rose tree one. But that's for later)

> data NanoCons s r n = NanoCons { -- s_ummary, r_efinement, n_anocube
>   nanoTail :: n,
>   refine :: Map.Map r (NanoCons s r n)
>   }
> data NanoNil s = NanoNil { nilSummary :: s }

Summaries should be members of a commutative monoid; the assumption is
that we're building summaries of a sample from a distribution.  We're
assuming the samples are independent of each other as well, and so
there's a monoid homomorphism from the likelihood function of the
sample to the monoid holding the summaries. Because the likelihood
function of IID samples is a product of the likelihood function of the
individual draws, and products are commutative, the summaries should be
commutative as well.

In other words, the "s" type parameter should be a monoid.

In addition, the "n" parameter should be a nanocube as well.

--------------------------------------------------------------------------------

Regions of a nanocube are addressed by multidimensional hierarchical addresses.
The type variable a denotes the type of the key of the hierarchy of the 
(Addr a t) type, and t is the tail of the het list.

> data Addr a t = MkAddr (DimAddr a) t deriving Show
> newtype DimAddr a = MkDimAddr { getDimAddr :: [a] } deriving (Show, Eq)

> addr1 :: DimAddr a -> Addr a ()
> addr1 a = MkAddr a ()
> addr2 :: DimAddr a -> DimAddr b -> Addr a (Addr b ())
> addr2 a b = MkAddr a (addr1 b)

--------------------------------------------------------------------------------

Singleton is a helper typeclass to build nanocubes with a single element.

> class (Singleton n s) where
>   singleton :: s -> n
> instance Singleton (NanoNil s) s where
>   singleton s = NanoNil s
> instance Singleton n s => Singleton (NanoCons s r n) s where
>   singleton s = NanoCons (singleton s) Map.empty

> class HasContent n s | n -> s where
>   content :: n -> s
> instance HasContent (NanoNil s) s where
>   content (NanoNil s) = s
> instance (HasContent n s) => HasContent (NanoCons s r n) s where
>   content (NanoCons tail _) = content tail

Nanocubes themselves are monoids, if you insist.

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
of using the monoid interface. We only use the monoid interface to build
nanocubes which store results from a query.

CanAdd encodes the main algorithm for building a nanocube, including
figuring out the shared substructures, courtesy of Haskell's persistent
data structures.

> class CanAdd s a n where
>   add :: a -> s -> n -> n
> instance (Eq s, Monoid s) => CanAdd s () (NanoNil s) where
>   add :: () -> s -> NanoNil s -> NanoNil s
>   add _ s (NanoNil s') = NanoNil (s' <> s)
> instance (Eq s, Monoid s, Ord r, CanAdd s a n, Monoid n) => CanAdd s (Addr r a) (NanoCons s r n) where
>   add :: Addr r a -> s -> NanoCons s r n -> NanoCons s r n
>   add address@(MkAddr (MkDimAddr []) ads) s (NanoCons next r) =
>        NanoCons (add ads s next) r
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

--------------------------------------------------------------------------------


