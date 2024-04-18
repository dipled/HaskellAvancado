{-# Language GADTSyntax #-}

{-
    Para instanciar um Monad, precisamos primeiro instanciar um Functor e um Applicative
-}

data Arvore a where
    No :: a -> Arvore a -> Arvore a -> Arvore a
    Folha :: Arvore a

instance Functor Arvore where
    fmap :: (a -> b) -> Arvore a -> Arvore b
    fmap f Folha = Folha
    fmap f (No a l r) = No (f a) (fmap f l) $ fmap f r

instance Applicative Arvore where
    pure :: a -> Arvore a
    pure x = No x Folha Folha

    (<*>) :: Arvore (a -> b) -> Arvore a -> Arvore b
    Folha <*> _ = Folha
    _ <*> Folha = Folha
    (No f l r) <*> (No a l' r') = (No (f a) (l <*> l') (r <*> r'))

instance Monad Arvore where
    (>>=) :: Arvore a -> (a -> Arvore b) -> Arvore b
    Folha 