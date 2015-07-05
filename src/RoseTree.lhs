> module RoseTree where
>
> import qualified Data.Map.Strict as Map
> 
> data RT k v = Leaf v
>             | RT (Map.Map k (RT k v)) deriving (Eq, Ord, Show)
