
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


