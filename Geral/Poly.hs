-- Polimorfismo universal ou paramétrico
-- o tipo de len' poderia ser escrito como ∀a, [a] -> Int
len' :: [a] -> Int
len' [] = 0
len' (x:xs) = 1 + len' xs 

-- Polimorfismo com restrições
-- Essas constraints sãos estabelecidas com classes de tipo, mas
-- poderíamos reescrever o tipo de somatorio como
-- ∀a, Num a -> [a] -> a, i.e, como se fosse uma implicação

somatorio :: Num a => [a] -> a
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

{- Polimorfismo Ad-Hoc ou Polimorfismo de sobrecarga:
    Em haskell, é implementado através das classes de tipo,
    como mostrado a seguir
-}

data Temperature where
  C :: Float -> Temperature
  F :: Float -> Temperature
 deriving(Show)

instance Eq Temperature where
  (==) (C n) (C m) = n == m
  (==) (F n) (F m) = n == m
  (==) (F n) (C m) = (1.8*m + 32) == n
  (==) (C n) (F m) = (1.8*n + 32) == m

{- Note que a função (==) foi implementada para o tipo
    Temperature, de forma a instanciá-lo como pertencente a
    classe Eq. Assim, sobrecarregamos a função (==) para esse tipo.
    Com isso, toda função que possuir um constraint do tipo Eq a poderá
    receber um dado do tipo Temperature como argumento.

-}





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