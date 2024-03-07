data RB = R | B
  deriving (Show)

data RBTree a = Node a RB (RBTree a) (RBTree a) | Leaf RB
  deriving (Show)

rot (Node x B a (Node y R b (Node z R c d))) = Node y R (Node x B a b) (Node z B c d)
rot (Node z B (Node y R (Node x R a b) c) d) = Node y R (Node x B a b) (Node z B c d)
rot (Node z B (Node x R a (Node y R b c)) d) = Node y R (Node x B a b) (Node z B c d)
