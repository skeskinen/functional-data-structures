module Sort.Merge where

import Sort.Sortable

data Sort a = Sort
  { less :: Less a
  , size :: Int
  , segments :: [[a]]
  }

merge :: Less a -> [a] -> [a] -> [a]
merge less = merge'
  where
    merge' [] ys = ys
    merge' xs [] = xs
    merge' xss@(x : xs) yss@(y : ys) = if less x y
      then x : merge' xs yss
      else y : merge' xss ys

instance Sortable Sort where
  new less = Sort less 0 []

  add a (Sort less size segs) =
    let addSeg seg segs size =
          if size `mod` 2 == 0
            then seg : segs
            else addSeg (merge less seg (head segs)) (tail segs) (size `div` 2)
    in Sort less (size + 1) (addSeg [a] segs size)

  sort (Sort less _ segs) = foldl (merge less) [] segs
