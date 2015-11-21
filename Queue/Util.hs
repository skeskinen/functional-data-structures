module Queue.Util where

import Test.QuickCheck

data QueueInstruction =
  Snoc !Int |
  Head |
  Tail
  deriving Show

instance Arbitrary QueueInstruction where
  arbitrary = do
    x <- choose (0,5) :: Gen Int
    return $ case x of
      0 -> Head
      1 -> Tail
      _ -> Snoc x
