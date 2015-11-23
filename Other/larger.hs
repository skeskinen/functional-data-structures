-- find the max amount of strictly larger trailing elements for [x | <- xs]

{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}
import Data.Char
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Arrow
import GHC.Exts (the)
import Data.List (group, sort, splitAt)
import Debug.Trace
import System.Random
import Control.Monad (replicateM)
import System.IO.Unsafe
import Data.Monoid
import Criterion.Main

-- n^2
largerQ :: Ord a => [a] -> Int
largerQ = maximum . slowLarger'
  where
    slowLarger' [] = []
    slowLarger' (x:xs) = larger' x xs : slowLarger' xs
    larger' x xs = length $ filter (>x) xs


-- n * m
-- m = length alphabet
largerSlow :: forall a. Ord a => [a] -> Int
largerSlow xs = maximum $ M.map larger' firsts
  where v = V.fromList xs
        firsts = foldr (uncurry M.insert) M.empty $ zip xs ([1..] :: [Int])
        larger' i = V.length $ V.filter (> v V.! (i - 1)) (snd (V.splitAt i v))

-- ugly n lg n
-- already fastests but could be optimized
largerIdx :: forall a. (Ord a, Show a) => [a] -> Int
largerIdx xs = thrd $ M.foldlWithKey' go (s, initR, initR) (M.deleteMin firsts)
  where thrd (_,_,a) = a
        initR :: Int
        initR = larger' y
        v = V.fromList xs
        first :: a -> Int
        first a = M.findWithDefault (-1) a firsts
        firsts = foldr (uncurry M.insert) M.empty $ zip xs ([0..] :: [Int])
        counts = M.fromList . map (\a -> (the a, length a)) $ group (sort xs)
        larger' i = V.length $ V.filter (> v V.! i) (snd (V.splitAt i v))
        (s, y) = M.findMin firsts
        count :: a -> Int
        count a = M.findWithDefault 0 a counts
        go :: (a, Int, Int) -> a -> Int -> (a, Int, Int)
        go (s, m, r) k b = if b < s' then (k, newR, max newR r)
                                     else (s, m - count k, r)
          where
            s' = first s
            newR = m + s' - count k - b

-- another version of the above
-- not much cleaner n lg n
-- slightly slower? not much
largerIdxMemo :: forall a. (Ord a, Show a) => [a] -> Int
largerIdxMemo xs = thrd $ M.foldl' go (first, initR, initR) (M.deleteMin memo)
  where thrd (_,_,a)= a
        memo :: M.Map a Memo
        memo = foldr insertMemo M.empty $ zip xs ([0..] :: [Int])
        insertMemo = (\(a, i) -> M.insertWith addMemo a (newMemo i))
        (firstA, first) = M.findMin memo
        initR = length . filter (/=firstA) . dropWhile (/=firstA) $ xs
        go :: (Memo, Int, Int) -> Memo -> (Memo, Int, Int)
        go (o@(Memo _ oldI), m, r) n@(Memo newC newI) = if newI < oldI
                                                            then (n, newR, max newR r)
                                                            else (o, m - newC, r)
          where
            newR = m + oldI - newC - newI

-- n * (m * lg m)
largerMapQ :: forall a. (Ord a, Show a) => [a] -> Int
largerMapQ xs = fst $ foldr go (0, M.empty) xs
  where
    go :: a -> (Int, M.Map a Int) -> (Int, M.Map a Int)
    go x (r, m) = (max r (count x m), M.insertWith (+) x 1 m)
    count x m = M.foldr (+) 0 (snd $ M.split x m)

-- efficient version of the above
-- n lg n
-- tree data-structure at the end of the file
largerSegment :: forall a. (Ord a, Eq a, Show a) => [a] -> Int
largerSegment xs = fst $ foldr go (0, tree) xs
  where
    go :: a -> (Int, Tree a) -> (Int, Tree a)
    go x (r, m) = (max r (sumMax x m), update x m)
    tree = fromOrdList . map the . group . sort $ xs

-- n^2
xciLargerQ :: Ord a => [a] -> Int
xciLargerQ = maximum . map snd . foldm1 joinQ . map xciSingleton
  where
    joinQ :: Ord a => [J a] -> [J a] -> [J a]
    joinQ [] ys = ys
    joinQ xs [] = xs
    joinQ xs'@((x,c) : xs) ys'@((y,d) : ys)
      | x < y = (x, c + length ys') : joinQ xs ys'
      | otherwise = (y,d) : joinQ xs' ys

-- n lg n
xciLarger :: Ord a => [a] -> Int
xciLarger xs = maximum . map snd . fst . go (length xs) . map xciSingleton $ xs
  where
    go 1 (x:xs) = (x, xs)
    go n as = let
      m = n `div` 2
      (a1, as1) = go (n - m) as
      (a2, as2) = go m as1
      in (join m a1 a2, as2)
    join _ [] ys = ys
    join _ xs [] = xs
    join n xs'@((x,c) : xs) ys'@((y,d): ys)
      | x < y = (x, c + n) : join n xs ys'
      | otherwise = (y,d) : join (n-1) xs' ys

data Memo = Memo Int Int
count (Memo c _) = c
index (Memo _ i) = i
addMemo (Memo c i) (Memo c' _) = Memo (c + c') i
newMemo = Memo 1

foldm1 :: (a -> a -> a) -> [a] -> a
foldm1 f [] = error "empty"
foldm1 f (x:[]) = error "need 2 elems for foldm1"
foldm1 f as = fst $ go (length as) as
  where
    go 1 (a:as) = (a, as)
    go n as = let
      m = n `div` 2
      (a1, as1) = go (n-m) as
      (a2, as2) = go m as1
      in (f a1 a2, as2)

data Tree a = L Int a | N Int (Tree a) a a (Tree a)
  deriving Show

sample = "GENERATING"

randomSample i = replicateM i $ randomRIO (1,1000)

sumMax :: Ord a => a -> Tree a -> Int
sumMax a (L s b)
  | a < b = s
  | otherwise = 0
sumMax a (N s l min split r)
  | a < min = s
  | a > split = sumMax a r
  | otherwise = sumMax a l + sumMax a r

singleton :: a -> Tree a
singleton a = L 0 a

merge :: Tree a -> Tree a -> Tree a
merge l@(L _ la) r@(L _ ra) = N 0 l la ra r
merge l@(N _ _ m _ _) r@(L _ ra) = N 0 l m ra r
merge l@(N _ _ lm _ _) r@(N _ _ rm _ _) = N 0 l lm rm r

fromOrdList :: [a] -> Tree a
fromOrdList xs = foldm1 merge $ map singleton xs

update :: Ord a => a -> Tree a -> Tree a
update a l@(L s b)
  | a == b = L (s+1) a
  | otherwise = error "wut?"
update a n@(N s l m sp r)
  | a >= sp = N (s+1) l m sp (update a r)
  | otherwise = N (s+1) (update a l) m sp r

type J a = (a, Int)

xciSingleton :: a -> [J a]
xciSingleton c = [(c, 0)]

main = do
  xs <- randomSample 50000
  defaultMain $ [
      bench "index hack" $ nf largerIdx (xs :: [Int])
    , bench "segment tree" $ nf largerSegment xs
    , bench "xci join" $ nf xciLarger xs
    , bench "quadratic" $ nf largerQ xs
    ]
  putStrLn $ "All equal? " ++
    show (largerIdx xs == largerSegment xs && largerIdx xs == xciLarger xs)
