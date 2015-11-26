module Other.StructuralAbstraction where

import qualified Queue.Lazy as Q

data Cat a = Empty | Cat a (Q.Queue (Cat a))
  deriving Show

singleton a = Cat a Q.empty

empty :: Cat a
empty = Empty

cons :: a -> Cat a -> Cat a
cons a as = singleton a `cat` as

cat :: Cat a -> Cat a -> Cat a
cat Empty a = a
cat a Empty = a
cat (Cat a as) b = Cat a (Q.snoc as b)

head :: Cat a -> a
head Empty = error "empty"
head (Cat a _) = a

tail :: Cat a -> Cat a
tail Empty = error "empty"
tail (Cat a q)
  | Q.isEmpty q = Empty
  | otherwise = linkAll q

linkAll :: Q.Queue (Cat a) -> Cat a
linkAll q =
  let h = Q.head q
      t = Q.tail q
  in h `cat` linkAll t
