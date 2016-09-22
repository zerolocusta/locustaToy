module MyTree where

data Tree a = Leaf a
            | Node a (Tree a) (Tree a)
            deriving (Show)


leftMost :: Tree a -> a
leftMost (Node _ left _) = leftMost left
leftMost (Leaf v) =v

myMap :: (a -> b) -> Tree a -> Tree b
myMap f (Leaf v)  = Leaf $ f v
myMap f (Node n l r) = (Node (f n) (myMap f l) (myMap f r))
