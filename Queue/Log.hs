module Queue.Log where

import Queue.Util
import Prelude hiding (tail, head)

data Queue a = Queue [a] !Int [a] !Int

rotate [] (x:_) a = x : a
rotate (f:fs) (r:rs) a = f : rotate fs rs (r : a)

queue f lenF r lenR
  | lenR == lenF + 1 = Queue (rotate f r []) (lenF + lenR) [] 0
  | otherwise = Queue f lenF r lenR

empty = Queue [] 0 [] 0

isEmpty (Queue _ lenF _ _) = lenF == 0

snoc (Queue f lenF r lenR) x = queue f lenF (x : r) (lenR + 1)

head (Queue _ 0 _ _) = error "empty"
head (Queue (x:_) _ _ _) = x

tail (Queue _ 0 _ _) = error "empty"
tail (Queue (_:f) lenF r lenR) = queue f (lenF - 1) r lenR

eval :: [QueueInstruction] -> [Int]
eval = eval' empty
  where
    eval' (Queue f _ r _) [] = f ++ reverse r
    eval' q (x:xs) = case x of
      Snoc a -> eval' (snoc q a) xs
      Head -> head q : eval' q xs
      Tail -> eval' (tail q) xs
