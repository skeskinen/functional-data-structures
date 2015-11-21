module Queue.Lazy where

import Queue.Util
import Prelude hiding (tail, head)

data Queue a = Queue [a] [a]

queue [] r = Queue (reverse r) []
queue f r = Queue f r

empty = Queue [] []

isEmpty (Queue f _) = null f

snoc (Queue f r) x = queue f (x : r)

head (Queue [] _) = error "empty"
head (Queue (x:_) _) = x

tail (Queue [] _) = error "empty"
tail (Queue (_:f) r) = queue f r

eval :: [QueueInstruction] -> [Int]
eval = eval' empty
  where
    eval' (Queue f r) [] = f ++ reverse r
    eval' q (x:xs) = case x of
      Snoc a -> eval' (snoc q a) xs
      Head -> head q : eval' q xs
      Tail -> eval' (tail q) xs
