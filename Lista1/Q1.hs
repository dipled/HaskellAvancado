{-# Language UnicodeSyntax #-}

module Q1 where


data Tree a = Node a (Tree a) (Tree a) | Leaf 
  deriving (Show)

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node a e d) = inOrder e ++ [a] ++ inOrder d


insertNode :: (Ord a) => a -> Tree a -> Tree a
insertNode e Leaf = Node e Leaf Leaf
insertNode e n@(Node x l r)
  |e == x = n
  |e > x = Node x l (insertNode e r)
  |otherwise = Node x (insertNode e l) r


removeNode :: (Ord a) => a -> Tree a -> Tree a
removeNode e Leaf = Leaf
removeNode e n@(Node x Leaf Leaf)
  |e == x = Leaf
  |otherwise = n 
removeNode e n@(Node x l Leaf)
  |e == x = l
  |e > x = n
  |e < x = Node x (removeNode e l) Leaf
removeNode e n@(Node x Leaf r)
  |e == x = r
  |e < x = n
  |e < x = Node x Leaf (removeNode e r)
removeNode e n@(Node x l r)
  |e > x = Node x l (removeNode e r)
  |e < x = Node x (removeNode e l) r
  |e == x = Node x' (removeNode x' l) r where Node x' l' r' = greatestNode l

--escolhi substituir a raiz pelo maior dos menores
greatestNode :: Tree a -> Tree a
greatestNode Leaf = Leaf
greatestNode n@(Node x l r) = 
  case r of
    Leaf -> n
    Node χ λ ρ -> greatestNode r 


main :: IO()
main = 
  do
    let testTree = Node 21 (Node 14 (Node 10 (Node 5 Leaf Leaf) (Node 13 (Node 12 (Node 11 Leaf Leaf) Leaf) Leaf)) (Node 18 (Node 15 Leaf Leaf) (Node 19 Leaf Leaf))) (Node 28 (Node 25 Leaf Leaf) Leaf)
    print (removeNode 14 testTree)
