> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
> {-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}
>
> module Spatial where
>
> import qualified Data.Map.Strict as Map
> import Nanocube
> import Data.Maybe
> import Debug.Trace
> import Test.QuickCheck
> 
> class NCDim a b | a -> b where
>   dimAddr :: a -> DimAddr b

A "Bidant" is the 1-dimensional equivalent of a quadrant.

> data Quadrant = NW | NE | SW | SE deriving (Eq, Show, Enum, Ord, Bounded)
> type Bidant = Bool

> newtype NCUnitSquare a = MkUS (a, a) deriving Show
> newtype NCUnitInterval a = MkUI a deriving Show

> instance (Floating a, RealFrac a) => NCDim (NCUnitSquare a) Quadrant where
>   dimAddr :: NCUnitSquare a -> DimAddr Quadrant
>   dimAddr (MkUS (x, y)) = MkDimAddr $ map whichQuadrant $ quadTreeZip x y
>     where
>       whichQuadrant :: (Bool, Bool) -> Quadrant
>       whichQuadrant (False, False) = SW
>       whichQuadrant (True,  False) = SE
>       whichQuadrant (False, True)  = NW
>       whichQuadrant (True,  True)  = NE
>       quadTreeZip v1 v2 = zip (digits $ frac v1) (digits $ frac v2)

> instance (Floating a, RealFrac a) => NCDim (NCUnitInterval a) Bidant where
>   dimAddr :: NCUnitInterval a -> DimAddr Bidant
>   dimAddr (MkUI x) = MkDimAddr $ digits x

Some type synonyms for convenience

> type NC2D' s n = NanoCons s Quadrant n
> type NC1D' s n = NanoCons s Bidant n

--------------------------------------------------------------------------------

Interval utilities

We encode intervals as (Floating a, RealFrac a) => (a,a)

> bisect (mn, mx) = ((mn, mid), (mid, mx)) where mid = (mn + mx) / 2
> (lmn, lmx) `outsideOf` (rmn, rmx) = lmx <= rmn || lmn >= rmx
> (lmn, lmx) `insideOf` (rmn, rmx) =
>   lmn >= rmn && lmn <= rmx && lmx >= rmn && lmx <= rmx

--------------------------------------------------------------------------------

> newtype Region a = MkRegion {region :: Either () (Map.Map a (Region a))} deriving (Eq, Show)

visitRegion does a CPS-transformed enumeration of all the NanoCons
cells from c contained entirely inside r.

> type KL a = [a] -> [a]
> 
> visitRegion :: (Eq r, Ord r) => NanoCons s r n -> Region r -> [NanoCons s r n]
> visitRegion c r = (visitRegion' id c r) [] where
>   rejigger :: (a -> b -> c -> d) -> (a -> (b, c) -> d)
>   rejigger f p1 = uncurry (f p1)
>   visitRegion' :: (Eq r, Ord r) => (KL (NanoCons s r n)) -> NanoCons s r n -> Region r -> (KL (NanoCons s r n))
>   visitRegion' k c (MkRegion (Left ())) = \l -> c : (k l)
>   visitRegion' k c (MkRegion (Right regionRefine)) = Prelude.foldr ($) k childrenConts where
>     childrenConts = Prelude.map (\(c, r) k -> (rejigger visitRegion') k (c, r)) children
>     children = let (NanoCons next nanoRefine) = c
>                    -- lookup is in the Maybe monad, so returns Nothing if either
>                    -- Map.lookup fails
>                    lookup k = do r <- Map.lookup k regionRefine
>                                  n <- Map.lookup k nanoRefine
>                                  return (n, r)
>                 in catMaybes $ map lookup $ Map.keys regionRefine

> class HasTypeSize a where
>   typeSize :: a -> Int
>
> instance (Enum a, Bounded a) => HasTypeSize a where
>   typeSize i = 1 + fromEnum (maxBound `asTypeOf` i)

simplifyRegion simplifies regions specifically by ensuring that they
have bounded depth, and that their representation is minimal (so the
total number of leaves is smallest). This makes aggregation queries
faster, which might matter when dealing with monoids with relatively
expensive sum operators.

cropRegion replaces subregions that are too deep with empty space,
while coverRegion replaces those subregions with fully covered space.

> cropRegion, coverRegion :: (Eq a, Enum a, Bounded a) => Int -> Region a -> Region a
> cropRegion = simplifyRegion (MkRegion $ Right Map.empty)
> coverRegion = simplifyRegion (MkRegion $ Left ())

Note the seriously stupid "size" variable, which is a terrible
contortion to get the size of an enum type.

> simplifyRegion :: (Eq a, Enum a, Bounded a) => Region a -> Int -> Region a -> Region a
> simplifyRegion replacement 0 r@(MkRegion (Left ())) = r
> simplifyRegion replacement 0 r@(MkRegion (Right m))
>   | m == Map.empty = r
>   | otherwise = replacement
> simplifyRegion replacement n (MkRegion r)
>   | n < 0     = error "limitResolution' needs a positive cutoff"
>   | otherwise = MkRegion $ case r of
>         Left () -> Left ()
>         Right m -> let newMap = Map.filter (\v -> v /= MkRegion (Right Map.empty)) $ fmap (simplifyRegion replacement (n - 1)) m
>                        size = maybe (-1) typeSize $ listToMaybe (Map.keys m)
>                        -- I just need a witness of type a for this, because of
>                        -- that HasTypeSize class. This is stupid.
>                     in if (Map.size newMap /= size)
>                        then Right newMap
>                             else if all (\x -> x == MkRegion (Right Map.empty)) newMap
>                                  then Right Map.empty
>                                  else if all (\x -> x == MkRegion (Left ())) newMap
>                                       then Left ()
>                                       else Right newMap

--------------------------------------------------------------------------------

The type "Region Quadrant" is used to encode a subset of [0,1]^2.

Examples:

- The entire region is encoded as "Left ()".
- The empty region is encoded as "Right Map.empty"
- The region [0, 0.5] x [0, 0.5] is encoded as "Right $ Map.assocs [(SW, Left ())]"
- The region [0.5, 1] x [0, 0.5] is encoded as "Right $ Map.assocs [(SE, Left ())]"
- The region [0, 1] x [0, 0.5] is encoded as "Right $ Maps.assocs [(SW, Left ()), (SE, Left ())]"

In other words, the "current region" is represented implicitly by the
path taken in the keys of the maps; if the value is "Left ()", then
that entire "current region" is part of the whole region; if the value
is "Right Map ...", then the region is defined by the union of the
subregions in the elements of the map.

This representation is not unique, and so we don't define equality:
"Left ()" and "Right $ Map.assocs $ map (\x -> (x, Left ())) [SW, SE,
NW, NE]" are the same region, as are "Right $ Map.empty" and "Right $
Map.assocs $ map (\x -> (x, Right Map.empty)) [SW, SE, NW, NE]".

In general, regions will not have finite representations, so Eq is a
bad idea anyway.

> -- quadrantCover assumes that mnX < mxX, and mnY < mxY
> quadrantCover :: (Floating a, RealFrac a, Show a) => (a, a) -> (a, a) -> Region Quadrant
> quadrantCover queryX queryY =
>     quadrantCover' ((0, 1), (0, 1))
>   where
>     quadrantCover' (regionX, regionY)
>       | insideX   && insideY  = MkRegion $ Left ()
>       | outsideX  || outsideY = MkRegion $ Right $ Map.empty
>       | otherwise             = newRegion $ zip keys children
>       where
>         insideX  = regionX `insideOf` queryX 
>         insideY  = regionY `insideOf` queryY
>         outsideX = regionX `outsideOf` queryX 
>         outsideY = regionY `outsideOf` queryY
>         (leftX, rightX) = bisect regionX
>         (leftY, rightY) = bisect regionY
>         children = map quadrantCover' [(leftX,  leftY),
>                                        (rightX, leftY),
>                                        (leftX,  rightY),
>                                        (rightX, rightY)]
>         keys = [SW, SE, NW, NE]

--------------------------------------------------------------------------------

The same general idea of "Region Quadrant" applies for regions in
other dimensions.

Region Bidant, specifically, encodes a subset of [0,1], which we'll
use to encode one-dimensional columns.

> bidantCover :: (Floating a, RealFrac a) => (a, a) -> Region Bidant
> bidantCover query = bidantCover' (0, 1) where
>   bidantCover' interval
>     | inside    = MkRegion $ Left ()
>     | outside   = MkRegion $ Right $ Map.empty
>     | otherwise = newRegion $ zip keys children
>     where
>       inside  = interval `insideOf` query
>       outside = interval `outsideOf` query
>       (left, right) = bisect interval
>       children = map bidantCover' [left, right]
>       keys = [False, True]

--------------------------------------------------------------------------------
Utility

> newRegion :: (Ord a, Eq a) => [(a, Region a)] -> Region a
> newRegion = MkRegion . Right . Map.fromList .
>   filter (\ (_, v) -> v /= (MkRegion (Right Map.empty)))

> truncateDim :: Int -> DimAddr a -> DimAddr a
> truncateDim n (MkDimAddr l) = MkDimAddr $ take n l

> frac :: RealFrac a => a -> a
> frac x = x - (fromIntegral $ (floor x :: Integer))
  
> digits :: RealFrac a => a -> [Bool]
> digits x | x >= 0.5  = True  : (digits $ x * 2 - 1)
>          | otherwise = False : (digits $ x * 2)

> fromDigits :: [Bool] -> Double
> fromDigits lst = fromDigits' 0 0.5 lst
>   where
>     fromDigits' accum weight [] = accum
>     fromDigits' accum weight (el:els) =
>       fromDigits' (accum + (if el then weight else 0)) (weight/2) els

--------------------------------------------------------------------------------
Testing

> numbersInUnitInterval = choose (0.0, 1.0)
> prop_digits depth = forAll numbersInUnitInterval $
>     \number -> depth >= 0 ==> (number - fromDigits (take depth $ digits number)) <= 2 ** (negate $ fromIntegral depth)
>   where
>     types = depth :: Int

> testPoint :: (Floating a, RealFrac a) => (NCUnitSquare a) -> Region Quadrant -> Bool
> testPoint p r = let (MkDimAddr lst) = dimAddr p
>                 in testPoint' lst r
>   where
>     testPoint' _ (MkRegion (Left ())) = True
>     testPoint' (el:els) (MkRegion (Right m)) =
>       maybe False (\next -> testPoint' els next) (Map.lookup el m)

> genQuadrantLeaf :: Gen (Region Quadrant)
> genQuadrantLeaf = oneof [return $ MkRegion $ Left (), return $ MkRegion $ Right Map.empty]
>
> genQuadrant = sized genQuadrant'
> genQuadrant' 0 = genQuadrantLeaf
> genQuadrant' n | n > 0 =
>                  oneof [genQuadrantLeaf,
>                         do pairs <- sequence $ map genPair [minBound..maxBound]
>                            return $ MkRegion $ Right $ Map.fromList pairs]
>    where
>      genPair key = do v <- genQuadrant' (n `div` 4)
>                       return (key, v)

> pointsInUnitSquare = do a <- numbersInUnitInterval
>                         b <- numbersInUnitInterval
>                         return $ MkUS (a, b)
>
> instance Arbitrary (Region Quadrant) where
>   arbitrary = genQuadrant

> prop_coverSimplify region depth = forAll pointsInUnitSquare $
>   \point -> depth >= 0 && testPoint point region ==> testPoint point (coverRegion depth region)
>   where types = (region :: Region Quadrant,
>                  depth :: Int)
> prop_coverSimplify2 region depth = forAll pointsInUnitSquare $
>   \point -> depth >= 0 && (not $ testPoint point (coverRegion depth region)) ==> not $ testPoint point region
>   where types = (region :: Region Quadrant,
>                  depth :: Int)
> prop_cropSimplify region depth = forAll pointsInUnitSquare $
>   \point -> depth >= 0 && testPoint point (cropRegion depth region) ==> testPoint point region
>   where types = (region :: Region Quadrant,
>                  depth :: Int)
> prop_cropSimplify2 region depth = forAll pointsInUnitSquare $
>   \point -> depth >= 0 && (not $ testPoint point region) ==> not $ testPoint point (cropRegion depth region)
>   where types = (region :: Region Quadrant,
>                  depth :: Int)
> testSpatial = do quickCheck prop_cropSimplify
>                  quickCheck prop_cropSimplify2
>                  quickCheck prop_coverSimplify
>                  quickCheck prop_coverSimplify2
>                  quickCheck prop_digits
