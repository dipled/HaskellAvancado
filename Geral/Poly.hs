--Polimorfismo universal ou universal

len' :: [a] -> Int
len' [] = 0
len' (x:xs) = 1 + len' xs 

--Polimorfismo com restrições

somatorio :: Num a => [a] -> a
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs


data Arvore a = Folha | No a (Arvore a) (Arvore a)


class Len a where
    len :: a -> Int

instance Len [b] where
    len [] = 0
    len (x:xs) = 1 + len xs

instance Len(Arvore b) where
    len Folha = 0
    len (No x l r) = 1 + len l + len r

instance Len (Maybe b) where
    len Nothing = 0
    len (Just a) = 1

foo :: Len a => a -> Int
foo x = len x * 10

instance Show (a) => Show (Arvore a) where


    show Folha = "-"
    show (No x l r) = "(" ++ show l ++ show ")" ++ show x ++ show "(" ++ show r ++ show ")"