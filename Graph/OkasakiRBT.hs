module Graph.OkasakiRBT where

import Control.DeepSeq

data Color = R | B
data Tree a = E | T Color (Tree a) a (Tree a)

instance NFData a => NFData (Tree a) where
  rnf E = ()
  rnf (T _ l x r) = rnf x `seq` rnf l `seq` rnf r

empty :: Tree a
empty = E

member :: Ord a => a -> Tree a -> Bool
member x E = False
member x (T _ l y r) = case compare x y of
  LT -> member x l
  EQ -> True
  GT -> member x r

insert :: Ord a => a -> Tree a -> Tree a
insert x s = makeBlack (ins s)
  where
    ins E = T R E x E
    ins (T color l y r) = case compare x y of
      LT -> balance color (ins l) y r
      EQ -> T color l y r
      GT -> balance color l y (ins r)
    makeBlack (T _ l y r) = T B l y r

balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color l x r = T color l x r
