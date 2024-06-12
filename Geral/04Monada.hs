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
          ∀a, b, c (a . (b . c) = (a . b) . c)
          ∃a ∀e(a . e = a = e . a)
 Mônada: 
          return a >>= λb.n = n[a/b]
          m >>= λa.return a = m
          m >>= (λa.n >>= λb.o)= (m >>= λa.n) >>= λb.o
-}

data Log a = MLog (String, a) deriving Show

instance Functor Log where
    fmap :: (a -> b) -> Log a -> Log b
    fmap f (MLog (s, x)) = MLog (s, f x)

instance Applicative Log where
    (<*>) :: Log (a -> b) -> Log a -> Log b
    MLog (s, f) <*> MLog (s', a) = MLog (s <> s', f a)

    pure :: a -> Log a
    pure a = MLog ("", a)

instance Monad Log where
    (>>=) ::  Log a -> (a -> Log b) -> Log b
    MLog (s, a) >>= f = let MLog (s', b) = f a in MLog (s <> s', b)

log' :: String -> Log ()
log' s = MLog (s <> "\n", ())

{- State Monad -}

type Estado = Int
data SM a where
    MS :: (Estado -> (a, Estado)) -> SM a 


instance Functor SM where
    fmap :: (a -> b) -> SM a -> SM b
    fmap f (MS m) = MS (\e -> let (a, e') = m e in (f a, e'))

instance Applicative SM where
    pure :: a -> SM a
    pure x = MS (\e -> (x, e))

    (<*>) :: SM (a -> b) -> SM a -> SM b
    MS f <*> MS x = MS (\e -> let (f', e') = f e in let (a, e'') = x e' in (f' a, e''))

instance Monad SM where
    (>>=) :: SM a -> (a -> SM b) -> SM b
    MS m >>= f = MS (\e -> let (a, e') = m e in let MS fa = f a in fa e')

contador = MS (\x -> ((), x + 1))

{- mdc sem Mônada de Estado -}
mdc' :: (Num a, Ord a) => a -> a -> a
mdc' a b 
    | a == b = a
    | a > b = mdc' (a - b) b
    | otherwise = mdc' a (b - a)

{- mdc com Mônada de Estado -}
mdc :: (Num a, Ord a) => a -> a -> SM a
mdc a b 
    | a == b = do contador; return a
    | a > b = do contador; mdc (a - b) b
    | otherwise = do contador ; mdc a (b - a)

runMS :: SM a -> (a, Estado)
runMS (MS m) = m 0