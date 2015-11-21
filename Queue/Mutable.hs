module Queue.Mutable where

import Queue.Util
import Prelude hiding (tail, head)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.ST

data Queue s a = Queue (M.MVector s a) !Int !Int !Int !Int

empty :: ST s (Queue s Int)
empty = do
  v <- M.unsafeNew 256
  return $ Queue v 256 0 0 0

snoc :: Queue s Int -> Int -> ST s (Queue s Int)
snoc (Queue v l u h t) a
  | l == u = do
    v' <- M.unsafeNew (l * 2)
    let s = M.unsafeSlice h (l - h) v
    let t = M.unsafeSlice 0 (l - h) v'
    M.unsafeCopy t s
    when (h > 0) $ do
      let s = M.unsafeSlice 0 h v
          t = M.unsafeSlice (l - h) h v'
      M.unsafeCopy t s
    M.unsafeWrite v' u a
    return $ Queue v' (l * 2) (u + 1) 0 (u + 1)
  | otherwise = Queue v l (u + 1) h (if t + 1 >= l then 0 else t + 1) <$ M.unsafeWrite v t a

head :: Queue s a -> ST s a
head (Queue v _ _ h _) = M.read v h

tail :: Queue s a -> ST s (Queue s a)
tail (Queue v l u h t) = return $ Queue v l (u - 1) (if h + 1 >= l then 0 else h + 1) t

toList :: Queue s a -> ST s [a]
toList (Queue v l u h t) =
  if h < t
    then getList h u v
    else (++) <$> getList h (l - h) v <*> getList 0 t v
  where
    getList s c v = V.toList <$> V.unsafeFreeze (M.unsafeSlice s c v)

eval :: [QueueInstruction] -> [Int]
eval xs = runST $ do
  q <- empty
  eval' q xs
  where
    eval' q [] = toList q
    eval' q (x:xs) = case x of
      Snoc a -> snoc q a >>= \q' -> eval' q' xs
      Head -> (:) <$> head q <*> eval' q xs
      Tail -> tail q >>= \q' -> eval' q' xs
