> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
> {-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeOperators #-}

> module RoseTree where
>
> import Data.Map.Strict as Map
> import Data.Traversable
> import Data.Foldable
> import Data.Monoid
> 
> data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
> instance Functor Tree where
>   fmap f Empty = Empty
>   fmap f (Leaf x) = Leaf (f x)
>   fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)
> instance Foldable Tree where
>   foldMap f Empty = mempty
>   foldMap f (Leaf x) = f x
>   foldMap f (Node l k r) = foldMap f l <> f k <> foldMap f r
> instance Traversable Tree where
>    traverse f Empty = pure Empty
>    traverse f (Leaf x) = Leaf <$> f x
>    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
> 
> data RT k v = RTLeaf v
>             | RT (Map k (RT k v)) deriving (Eq, Ord, Show)
> instance Functor (RT k) where
>   fmap f (RTLeaf v)     = RTLeaf (f v)
>   fmap f (RT m)         = RT (fmap (fmap f) m)
> instance Foldable (RT k) where
>   foldMap f (RTLeaf v)  = f v
>   foldMap f (RT m)      = foldMap (foldMap f) m
> instance Traversable (RT k) where
>   traverse f (RTLeaf v) = RTLeaf <$> f v
>   traverse f (RT m)     = RT <$> traverse (traverse f) m
>
> test1 :: RT Int Int                                               
> test1 = RT (fromList [(1, RTLeaf 2), (2, RTLeaf 3), (3, RT (fromList [(0, RTLeaf 1)]))])

> newtype MkCons a = MkCons ([a] -> [a])
> instance Monoid (MkCons a) where
>   mempty                          = MkCons $ id
>   (MkCons a) `mappend` (MkCons b) = MkCons $ (\l -> a (b l))
> 
> toConcat :: a -> MkCons a
> toConcat x = MkCons $ \l -> x : l
>
> runConcat :: MkCons a -> [a]
> runConcat (MkCons f) = f []

> fancyConcat :: RT k v -> [v]
> fancyConcat rt = (fancyConcat' id rt) [] where
>   fancyConcat' :: ([v] -> [v]) -> RT k v -> ([v] -> [v])
>   fancyConcat' k (RTLeaf v) = \l -> v : (k l)
>   fancyConcat' k (RT m)     = Prelude.foldr ($) k childrenConts
>     where
>       childrenConts = Prelude.map (\c k -> fancyConcat' k c) (elems m)
