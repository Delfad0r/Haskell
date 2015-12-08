{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

import RangeTree

type Value = Sum Int
type Update = Sum Int
instance Updatable Value Update where
    applyUpdate (Sum u) (l, r) (Sum v) = Sum $ v + u * (fromIntegral (r - l))

solve :: IO ()
solve = do
    [n, q] <- map fst . mapMaybe BS.readInt . BS.words <$> BS.getLine
    queries <- sequence . replicate q $
        map fst . mapMaybe BS.readInt . tail . BS.words <$> BS.getLine
    BS.putStrLn . BS.unlines . map (BS.pack . show . getSum) . catMaybes $ answer (new (0, n)) queries
    where
        answer :: DRangeTree Int (Sum Int) (Sum Int) -> [[Int]] -> [Maybe (Sum Int)]
        answer _ [] = []
        answer rt (q : qs) = case q of
            [l, r]      -> (Just $ query (l - 1, r) rt) : answer rt qs
            [l, r, v]   -> Nothing : answer (update (Sum v) (l - 1, r) rt) qs

main = do
    t <- fst . fromJust . BS.readInt <$> BS.getLine
    sequence_ $ replicate t solve
