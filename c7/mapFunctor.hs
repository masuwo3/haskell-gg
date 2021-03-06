import Data.Map as DataMap
import Data.List as DataList

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b

instance (Ord k) => MyFunctor (DataMap.Map k) where
    myfmap f x = DataMap.fromList $ DataList.map (\(p,q) ->(p,f q)) $ DataMap.toList x