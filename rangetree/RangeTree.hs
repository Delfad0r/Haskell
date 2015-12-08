{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module RangeTree
( SRangeTree
, DRangeTree
, Updatable
, applyUpdate
, new
, fromList
, query
, update
, flush
, toList
) where


import Data.Function (on)
import Data.List (genericLength)
import Data.Monoid


class (Integral i, Monoid a) => Node n i a | n -> a i where
    newNode :: (i, i) -> a -> n
    
    getValue :: n -> a
    
    getBounds :: n -> (i, i)
    
    propagate :: n -> n -> n
    propagate _ c = c


data SNode i a = SNode (i, i) a

instance (Integral i, Monoid a) => Node (SNode i a) i a where
    newNode = SNode
    
    getValue (SNode _ v) = v
    
    getBounds (SNode b _) = b


class (Monoid u) => Updatable a u | u -> a where
    applyUpdate :: Integral i => u -> (i, i) -> a -> a

data DNode i a u = DNode (i, i) a u

instance (Integral i, Monoid a, Updatable a u) => Node (DNode i a u) i a where
    newNode b v = DNode b v mempty
    
    getValue (DNode _ v _) = v
    
    getBounds (DNode b _ _) = b
    
    propagate (DNode _ _ u') c = updateNode u' c


data RangeTree n i a = Leaf n | Deep n (RangeTree n i a) (RangeTree n i a)

type SRangeTree i a = RangeTree (SNode i a) i a

type DRangeTree i a u = RangeTree (DNode i a u) i a
        


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
        in  (on (<>) $ query (b, e) . propagateToTree n) lt rt
    where
        n       = getRoot t
        (l, r)  = getBounds n
        mid     = (l + r) `div` 2

update :: (Node (DNode i a u) i a, Updatable a u) => u -> (i, i) -> DRangeTree i a u -> DRangeTree i a u
update u' (b, e) t
    | e <= l || r <= b  = t
    | b <= l && r <= e  = replaceRoot (updateNode u' n) t
    | otherwise         = let Deep _ lt rt = t in (on merge $ update u' (b, e) . propagateToTree n) lt rt
    where
        n@(DNode (l, r) _ u) = getRoot t


flush :: (Node (DNode i a u) i a, Updatable a u) =>  DRangeTree i a u -> SRangeTree i a
flush (Leaf (DNode b v _)) = Leaf $ SNode b v
flush (Deep n lt rt) = (on merge $ flush . propagateToTree n) lt rt


toList :: SRangeTree i a -> [a]
toList t = toListf t []
    where
        toListf (Leaf (SNode _ v))  = (v :)
        toListf (Deep _ lt rt)      = ((.) `on` toListf) lt rt


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


propagateToTree :: Node n i a => n -> RangeTree n i a -> RangeTree n i a
propagateToTree n t = replaceRoot (propagate n $ getRoot t) t


updateNode :: (Integral i, Updatable a u) => u -> DNode i a u -> DNode i a u
updateNode u' (DNode b v u) = DNode b (applyUpdate u' b v) $ u <> u'

