import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid

import RangeTree


newtype Value = Value (Int, Int, Int)

instance Monoid Value where
    mempty = Value (0, 0, 0)
    Value (a1, b1, c1) `mappend` Value (a2, b2, c2) = let t = min b1 c2 in Value (a1 + a2 + 2 * t, b1 + b2 - t, c1 + c2 - t)

main = do
    s <- map (\c -> if c == '(' then Value (0, 1, 0) else Value (0, 0, 1)) <$> getLine
    let rt :: SRangeTree Int Value
        rt = fromList s
    queries <- unfoldr (\a -> if null a then Nothing else Just $ splitAt 2 a) . map read . tail . words <$> getContents
    putStrLn $ unlines $ map (\[l, r] -> let Value (ans, _, _) = fst $ query (l - 1, r) rt in show ans) queries
