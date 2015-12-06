{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module RangeTree
( SRangeTree
, new
, fromList
, query
) where


import Data.Function (on)
import Data.List (genericLength)
import Data.Monoid


class (Integral i, Monoid a) => Node n i a | n -> a i where
    newNode :: (i, i) -> a -> n
    
    getValue :: n -> a
    
    getBounds :: n -> (i, i)
    
    propagate :: n -> n -> n
    propagate _ n = n


data SNode i a = SNode (i, i) a

instance (Integral i, Monoid a) => Node (SNode i a) i a where
    newNode = SNode
    
    getValue (SNode _ v) = v
    
    getBounds (SNode b _) = b


data RangeTree n i a = Leaf n | Deep n (RangeTree n i a) (RangeTree n i a)

type SRangeTree i a = RangeTree (SNode i a) i a


new :: Node n i a => (i, i) -> RangeTree n i a
new (l, r)
    | l >= r        = error "new: invalid bounds"
    | l + 1 == r    = Leaf $ newNode (l, r) mempty
    | otherwise     = Deep (newNode (l, r) mempty) (new (l, mid)) (new (mid, r))
    where
        mid = (l + r) `div` 2


fromList :: Node n i a => [a] -> RangeTree n i a
fromList [] = error "fromList: empty list"
fromList xs = fromList' xs (0, genericLength xs)
    where
        fromList' [x] (l, r)    = Leaf $ newNode (l, r) x
        fromList' xs (l, r)     = merge (fromList' lxs (l, mid)) (fromList' rxs (mid, r))
            where
                mid         = (l + r) `div` 2
                (lxs, rxs)  = splitAt (fromIntegral $ (r - l) `div` 2) xs


query :: Node n i a => (i, i) -> RangeTree n i a -> a
query (b, e) t
    | e <= l || r <= b  = mempty
    | b <= l && r <= e  = getValue n
    | otherwise         =
        let Deep _ lt rt    = t
            (lt', rt')      = (on (,) $ \t' -> replaceRoot (propagate n $ getRoot t') t') lt rt
        in  (on (<>) $ query (b, e)) lt' rt'
    where
        n       = getRoot t
        (l, r)  = getBounds n
        mid     = (l + r) `div` 2


merge :: Node n i a => RangeTree n i a -> RangeTree n i a -> RangeTree n i a
merge lt rt = Deep (newNode (l, r) (((<>) `on` getValue) ln rn)) lt rt
    where
        ln      = getRoot lt
        rn      = getRoot rt
        (l, _)  = getBounds ln
        (_, r)  = getBounds rn


getRoot :: RangeTree n i a -> n
getRoot (Leaf n) = n
getRoot (Deep n _ _) = n


replaceRoot :: n -> RangeTree n i a -> RangeTree n i a
replaceRoot r (Leaf _) = Leaf r
replaceRoot r (Deep _ lt rt) = Deep r lt rt



