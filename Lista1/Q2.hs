module Q2 where
import qualified Q1 

data RB = R | B
  deriving (Show)

data RBTree a = Node a RB (RBTree a) (RBTree a) | Leaf
  deriving (Show)

rot :: RBTree a -> RBTree a
rot (Node x B a (Node y R b (Node z R c d))) = Node y R (Node x B a b) (Node z B c d)
rot (Node z B (Node y R (Node x R a b) c) d) = Node y R (Node x B a b) (Node z B c d)
rot (Node z B (Node x R a (Node y R b c)) d) = Node y R (Node x B a b) (Node z B c d)
rot (Node z B a (Node y R (Node x R b c) d)) = Node y R (Node x B a b) (Node z B c d)
rot n = n

rootColor :: RBTree a -> RBTree a
rootColor Leaf = Leaf
rootColor (Node x R l r) = Node x B l r 
rootColor n@(Node x B l r ) = n 

insertNode' :: (Ord a) => a -> RBTree a -> RBTree a
insertNode' e Leaf = Node e R Leaf Leaf
insertNode' e n@(Node x c l r)
  |e == x = n
  |e > x = rot (Node x c l (insertNode' e r))
  |e < x = rot (Node x c (insertNode' e l) r)

insertNode :: (Ord a) => a -> RBTree a -> RBTree a
insertNode e a = rootColor(rot (insertNode' e a))

main :: IO()
main =
  do
    let testTree = insertNode 9 (insertNode 10 (insertNode 13 (insertNode 12 (insertNode 11 Leaf))))
    print testTree
