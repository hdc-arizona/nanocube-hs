> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
> {-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}
>
> module Spatial where
>
> import qualified Data.Map.Strict as Map
> import Nanocube
> import Data.Maybe
> 
> class NCDim a b | a -> b where
>   dimAddr :: a -> DimAddr b

> quadTreeZip v1 v2 = zip (digits $ frac v1) (digits $ frac v2)
>   where
>     digits :: RealFrac a => a -> [Bool]
>     digits x | x >= 0.5  = True  : rest
>              | otherwise = False : rest
>       where rest = digits $ x * 2
>
> data Quadrant = NW | NE | SW | SE deriving (Eq, Show, Enum, Ord)
> whichQuadrant :: (Bool, Bool) -> Quadrant
> whichQuadrant (False, False) = SW
> whichQuadrant (True,  False) = SE
> whichQuadrant (False, True)  = NW
> whichQuadrant (True,  True)  = NE

> newtype NCUnitSquare a = MkUS (a, a) deriving Show

> instance (Floating a, RealFrac a) => NCDim (NCUnitSquare a) Quadrant where
>   dimAddr :: NCUnitSquare a -> DimAddr Quadrant
>   dimAddr (MkUS (x, y)) = MkDimAddr $ map whichQuadrant $ quadTreeZip x y

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

The same general idea applies for regions in other dimensions

> newtype Region a = MkRegion (Either () (Map.Map a (Region a))) deriving Eq
> 
> limitResolution' :: Region a -> Int -> Region a -> Region a
> limitResolution' replacement 0 _ = replacement
> limitResolution' replacement n (MkRegion r)
>   | n < 0     = error "limitResolution' wants a positive cutoff"
>   | otherwise = MkRegion $ case r of
>         Left () -> Left ()
>         Right m -> Right $ fmap (limitResolution' replacement (n - 1)) m
>
> -- regionCover assumes that mnX < mxX, and mnY < mxY
> regionCover :: (Floating a, RealFrac a) => (a, a) -> (a, a) -> Region Quadrant
> regionCover queryX queryY =
>     regionCover' ((0, 1), (0, 1))
>   where
>     (lmn, lmx) `outsideOf` (rmn, rmx) = lmx <= rmn || lmn >= rmx
>     (lmn, lmx) `insideOf` (rmn, rmx) =
>         lmn >= rmn && lmn <= rmx && lmx >= rmn && lmx <= rmx
>     regionCover' (regionX, regionY)
>       | insideX     && insideY     = MkRegion $ Left ()
>       | outsideX    && outsideY    = MkRegion $ Right $ Map.empty
>       | otherwise                  = MkRegion $ Right $ Map.fromList $ zip keys children
>       where
>         insideX  = regionX `insideOf` queryX 
>         insideY  = regionY `insideOf` queryY
>         outsideX = regionX `outsideOf` queryX 
>         outsideY = regionY `outsideOf` queryY
>         (leftX, rightX) = bisect regionX
>         (leftY, rightY) = bisect regionY
>         children = map regionCover' [(leftX, leftY), (rightX, leftY),
>                                      (leftX, rightY), (rightX, rightY)]
>         keys = [SW, SE, NW, NE]


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

> bisect (mn, mx) = ((mn, mid), (mid, mx)) where mid = (mn + mx) / 2

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

-- this requires a 2D dimension
-- regionCover :: (Floating a, RealFrac a, Integral b) => (a, a) -> (a, a) -> [DimAddr b]
-- regionCover 


--------------------------------------------------------------------------------
Utility

> truncateDim :: Int -> DimAddr a -> DimAddr a
> truncateDim n (MkDimAddr l) = MkDimAddr $ take n l
> frac :: RealFrac a => a -> a
> frac x = x - (fromIntegral $ (floor x :: Integer))
