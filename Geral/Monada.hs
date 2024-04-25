{-# Language GADTSyntax #-}

{-
    Para instanciar um Monad, precisamos primeiro instanciar um Functor e um Applicative
-}

data Arvore a where
    No :: a -> Arvore a -> Arvore a -> Arvore a
    Folha :: Arvore a
    deriving (Show)

data Term where
    Const :: Float -> Term
    Add :: Term -> Term -> Term
    Sub :: Term -> Term -> Term
    Mul :: Term -> Term -> Term
    Div :: Term -> Term -> Term
    deriving (Show)
    
-- eval :: Term -> Float
-- eval (Add t1 t2) = eval t1 + eval t2
-- eval (Sub t1 t2) = eval t1 - eval t2
-- eval (Mul t1 t2) = eval t1 * eval t2
-- eval (Div t1 t2) = eval t1 / eval t2
-- eval (Const a) = a

{- Usando Monad Maybe para tratar divisao por zero -}
eval :: Term -> Maybe Float
eval (Const a) = return a
eval (Add a b) = do eval a >>= \x -> eval b >>= \y -> return $ x + y
eval (Sub a b) = do eval a >>= \x -> eval b >>= \y -> return $ x - y
eval (Mul a b) = do eval a >>= \x -> eval b >>= \y -> return $ x * y
eval (Div a b) = do eval a >>= \x -> eval b >>= \y -> if y /= 0 
                                                then return $ x / y
                                                else Nothing
{- 
 Monoide: Operação binária associativa com elemento neutro 
          ∀a,b,c(a.(b.c) = (a.b).c)
          ∃a∀e(a.e = a = e.a)
 Mônada: 
          return a >>= λb.n = n[a/b]
          m >>= λa.return a = m
          m >>= (λa.n >>= λb.o)= (m >>= λa.n) >>= λb.o
-}

data Log a = ML (String, a) deriving Show

instance Functor Log where
    fmap :: (a -> b) -> Log a -> Log b
    fmap f (ML (s, x)) = ML (s, f x)

instance Applicative Log where
    (<*>) :: Log (a -> b) -> Log a -> Log b
    ML (s, f) <*> ML (s', a) = ML (s <> s', f a)

    pure :: a -> Log a
    pure a = ML ("", a)

instance Monad Log where
    (>>=) ::  Log a -> (a -> Log b) -> Log b
    ML (s, a) >>= f = let ML (s', b) = f a in ML (s <> s', b)

log' :: String -> Log ()
log' s = ML (s <> "\n", ())

-- instance Functor Arvore where
--     fmap :: (a -> b) -> Arvore a -> Arvore b
--     fmap f Folha = Folha
--     fmap f (No a l r) = No (f a) (fmap f l) $ fmap f r

-- instance Applicative Arvore where
--     pure :: a -> Arvore a
--     pure x = No x Folha Folha

--     (<*>) :: Arvore (a -> b) -> Arvore a -> Arvore b
--     Folha <*> _ = Folha
--     _ <*> Folha = Folha
--     (No f l r) <*> (No a l' r') = (No (f a) (l <*> l') $ r <*> r')

-- instance Monad Arvore where
--     (>>=) :: Arvore a -> (a -> Arvore b) -> Arvore b
--     Folha >>= f = Folha
--     (No a l r) >>= f = (let (No b l' r') = f a in (No b (l >>= f) $ r >>= f))