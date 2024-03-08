data RB = R | B
  deriving (Show)

data RBTree a = Node a RB (RBTree a) (RBTree a) | Leaf RB
  deriving (Show)

rot :: RBTree a -> RBTree a
rot (Node x B a (Node y R b (Node z R c d))) = Node y R (Node x B a b) (Node z B c d)
rot (Node z B (Node y R (Node x R a b) c) d) = Node y R (Node x B a b) (Node z B c d)
rot (Node z B (Node x R a (Node y R b c)) d) = Node y R (Node x B a b) (Node z B c d)
rot (Node x B a (Node z R(Node y R b c)d)) = Node y R (Node x B a b) (Node z B c d)
rot n = n

rootColor :: RBTree a -> RBTree a
rootColor (Leaf _) = Leaf B 
rootColor (Node x R l r) = Node x B l r 
rootColor n@(Node x B l r ) = n 

insertNode' :: (Ord a) => a -> RBTree a -> RBTree a
insertNode' e (Leaf c) = Node e R (Leaf B) (Leaf B)
insertNode' e n@(Node x c l r)
  |e == x = n
  |e > x = rot (Node x c l (insertNode' e r))
  |e < x = rot (Node x c (insertNode' e l) r)

insertNode :: (Ord a) => a -> RBTree a -> RBTree a
insertNode e a = rootColor(rot (insertNode' e a))