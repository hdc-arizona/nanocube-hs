{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}

module Main where

import Query
import Spatial
import Schema
import Nanocube
import Text.JSON
import Data.Monoid
import RoseTree
import System.Random

main = putStrLn "hello"

newtype Point2 = MkPoint2 (Double, Double) deriving Show

instance Random Point2 where
  -- really, no monad here to help me?
  randomR (MkPoint2 (mnx, mny), MkPoint2 (mxx, mxy)) g =
    case randomR (mnx, mxx) g of
      (x, g') -> case randomR (mny, mxy) g' of
        (y, g'') -> (MkPoint2 (x, y), g'')
  random g = randomR (MkPoint2 (0.0, 0.0), MkPoint2 (1.0, 1.0)) g

randomPointsInCircle g = map shrink . filter check . map grow $ randoms g :: [Point2]
  where grow (MkPoint2 (x, y)) = MkPoint2 (x * 2 - 1, y * 2 - 1)
        check (MkPoint2 (x, y)) = x * x + y * y <= 1
        shrink (MkPoint2 (x, y)) = MkPoint2 (x / 2 + 0.5, y / 2 + 0.5)

randomNanocube :: Int -> IO (NC2D (Sum Int))
randomNanocube n = do g <- newStdGen
                      let ps = map (addr1 . truncateDim 8 . dimAddr . toUS) $ take n (randomPointsInCircle g)
                      return $ foldr (\p n -> add p (Sum 1 :: Sum Int) n) mempty ps
  where
    toUS (MkPoint2 a) = MkUS a
                      
--------------------------------------------------------------------------------
-- ... why don't people do this?

type a $ b = a b
infixr 0 $

addr :: [a] -> a' -> Addr a a'
addr k a = MkAddr (MkDimAddr k) a

p1'' = ()
p1' = addr [1 :: Int] p1''
p1 :: Addr Int $ Addr Int $ ()
p1 = addr [1, 2] p1'

p2 = MkAddr (MkDimAddr [1 :: Int,2 :: Int]) $ MkAddr (MkDimAddr [2 :: Int]) $ ()
p3 = MkAddr (MkDimAddr [1 :: Int,1 :: Int]) $ MkAddr (MkDimAddr [1 :: Int]) $ ()
p4 = MkAddr (MkDimAddr [1 :: Int,1 :: Int]) $ MkAddr (MkDimAddr [2 :: Int]) $ ()

u :: (Sum Int)
u = (Sum (1 :: Int))

c0'' :: NanoNil (Sum Int)
c0'' = mempty
c0' :: NanoCons (Sum Int) Int (NanoNil (Sum Int))
c0' = mempty

c1'' = add p1'' u c0''
c1' = add p1' u c0'

c0 :: NanoCons (Sum Int) Int $ NanoCons (Sum Int) Int $ NanoNil (Sum Int)
c0 = mempty
c1 = add p1 u c0
c2 = add p2 u c1
c3 = add p3 u c2
c4 = add p4 u c3

qsum :: [[a]] -> q -> SimpleQuery a q
qsum addrs q = Query QuerySum (MkSimpleQueryDim (map MkDimAddr addrs)) q

q0'' = ()
q0' :: SimpleQuery Int ()
q0' = qsum [[]] q0''
q0 :: SimpleQuery Int (SimpleQuery Int ())
q0 = qsum [[]] q0'

r0'' = query q0'' c0''

r0' = query q0' c1'

r0 = query q0 c4

instance JSON a => JSON (Sum a) where
  showJSON (Sum a) = showJSON a
  readJSON = undefined
