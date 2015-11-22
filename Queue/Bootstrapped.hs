module Queue.Bootstrapped where

import Queue.Util
import Prelude hiding (tail, head)

data Queue a = Empty | Queue [a] (Queue [a]) Int [a] Int

queue :: [a] -> Queue [a] -> Int -> [a] -> Int -> Queue a
queue f m lenFM r lenR
  | lenR <= lenFM = checkF f m lenFM r lenR
  | otherwise = checkF f (snoc m (reverse r)) (lenFM + lenR) [] 0
checkF [] Empty lenFM r lenR = Empty
checkF [] m lenFM r lenR = Queue (head m) (tail m) lenFM r lenR
checkF f m lenFM r lenR = Queue f m lenFM r lenR

empty = Empty

isEmpty Empty = True
isEmpty _ = False

snoc Empty x = Queue [x] Empty 1 [] 0
snoc (Queue f m lenFM r lenR) x = queue f m lenFM (x : r) (lenR + 1)

head Empty = error "empty"
head (Queue (x:_) _ _ _ _) = x

tail Empty = error "empty"
tail (Queue (_:f) m lenFM r lenR) = queue f m (lenFM - 1) r lenR

toList Empty = []
toList q = head q : toList (tail q)

eval :: [QueueInstruction] -> [Int]
eval = eval' empty
  where
    eval' q [] = toList q
    eval' q (x:xs) = case x of
      Snoc a -> eval' (snoc q a) xs
      Head -> head q : eval' q xs
      Tail -> eval' (tail q) xs
