module Queue.RealTime where

import Queue.Util
import Prelude hiding (tail, head)

data Queue a = Queue [a] [a] [a]

rotate [] (x:_) a = x : a
rotate (f:fs) (r:rs) a = f : rotate fs rs (r : a)

queue f r [] = let f' = rotate f r [] in Queue f' [] f'
queue f r (_ : s) = Queue f r s

empty = Queue [] [] []

isEmpty (Queue f _ _) = null f

snoc (Queue f r s) x = queue f (x : r) s

head (Queue [] _ _) = error "empty"
head (Queue (x:_) _ _) = x

tail (Queue [] _ _) = error "empty"
tail (Queue (_:f) r s) = queue f r s

eval :: [QueueInstruction] -> [Int]
eval = eval' empty
  where
    eval' (Queue f r _) [] = f ++ reverse r
    eval' q (x:xs) = case x of
      Snoc a -> eval' (snoc q a) xs
      Head -> head q : eval' q xs
      Tail -> eval' (tail q) xs
