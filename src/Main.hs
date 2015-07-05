{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}

module Main where

import Query
import Spatial
import Nanocube
import Text.JSON
import Data.Monoid
import RoseTree

main = putStrLn "hello"

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
qsum addrs q = SimpleQuery (SimpleQueryDim QuerySum (map MkDimAddr addrs)) q

q0'' = ()
q0' :: SimpleQuery Int ()
q0' = qsum [[]] q0''
q0 :: SimpleQuery Int $ SimpleQuery Int $ ()
q0 = qsum [[]] q0'

r0'' = query q0'' c0''

r0' = query q0' c1'

r0 = query q0 c4

instance JSON a => JSON (Sum a) where
  showJSON (Sum a) = showJSON a
  readJSON = undefined
