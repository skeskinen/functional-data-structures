module Sort.Heap where

data Heap a = E | H Int (Heap a) a (Heap a)
  deriving (Show, Eq)

heap :: Ord a => Heap a -> a -> Heap a -> Heap a
heap l x r
  | rank l < rank r = H (rank l + 1) r x l
  | otherwise = H (rank r + 1) l x r

empty = E

rank :: Heap a -> Int
rank E = 0
rank (H s _ _ _) = s

findMin :: Heap a -> a
findMin (H _ _ x _) = x

merge :: Ord a => Heap a -> Heap a -> Heap a
merge E r = r
merge l E = l
merge h1@(H _ a1 x b1) h2@(H _ a2 y b2)
  | x <= y = heap a1 x (merge b1 h2)
  | otherwise = heap a2 y (merge h1 b2)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = error "empty"
deleteMin (H _ l x r) = merge l r

insert :: Ord a => a -> Heap a -> Heap a
insert x E =  H 1 E x E
insert x h = merge (H 1 E x E) h
