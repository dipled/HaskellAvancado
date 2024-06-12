{-# LANGUAGE GADTs#-}

data Booleano = Verdadeiro | Falso


data Semana = Dom | Seg | Ter | Qua | Qui | Sex | Sab
  deriving (Show, Eq, Ord) -- Fala pro compilador gerar automaticamente uma definição da classe Show, Eq, Ord 
                           -- para o tipo de dado

proximo :: Semana -> Semana
proximo Seg = Ter
proximo Ter = Qua
proximo Qua = Qui
proximo Qui = Sex
proximo Sex = Sab
proximo Sab = Dom
proximo _ = Seg

proximo' :: Semana -> Semana
proximo' d =
  case d of
    Seg -> Ter
    Ter -> Qua
    Qua -> Qui
    Qui -> Sex
    Sex -> Sab
    Sab -> Dom

type Cod = Int

type Nome = String

type Email = String

data Cadastro = Pessoa Cod Nome Email
  deriving (Show)

procurar :: Cod -> [Cadastro] -> Nome
procurar cod [] = ""
procurar cod ((Pessoa c n e) : ps)
  | cod == c = n
  | otherwise = procurar cod ps

data Dupla a b = Par a b -- Dupla não é um tipo, mas sim um construtor de tipo
  deriving (Show)

primeiro :: Dupla a b -> a
primeiro (Par a _) = a

segundo :: Dupla a b -> b
segundo (Par _ b) = b

data Lista a = Elemento a (Lista a) | Nil
  deriving (Show)

tamanho :: (Num b) => Lista a -> b
tamanho Nil = 0
tamanho (Elemento a l) = 1 + tamanho l

data Arvore a = Folha | No a (Arvore a) (Arvore a)
  deriving (Show)

ins :: (Ord a) => a -> Arvore a -> Arvore a
ins n Folha = No n Folha Folha
ins n a@(No x e d) -- Esse @ indica que 'a' é equivalente a (No x e d)
  | n > x = No x e (ins n d)
  | n < x = No x (ins n e) d
  | otherwise = a

altura :: (Ord b) => (Num b) => Arvore a -> b
altura Folha = 0
altura (No a e d) = 1 + max (altura e) (altura d)

-- Exercicio: funcao que recebe uma arvore e retorna uma lista ordenada

inOrder :: Arvore a -> [a]
inOrder Folha = []
inOrder (No a e d) = inOrder e ++ [a] ++ inOrder d



-- data TermI = Lit Int | Succ TermI
--     deriving(Show)
-- 
-- data TermB = LitB Bool | IsZero TermI
--     deriving(Show)
-- 
-- data Term = If TermB Term Term | TB TermB | TI TermI
--     deriving(Show)
-- 
-- evalI :: TermI -> Int
-- evalI (Lit i) = i
-- evalI (Succ t) = 1 + evalI t
-- 
-- evalB :: TermB -> Bool
-- evalB (LitB b) = b
-- evalB (IsZero t) = 0 == evalI t
-- 
-- 
-- eval :: Term -> Term -- um lixo n faz o que a gente quer, entao o sei la quem fodao de haskell apresentou um negócio doidera:
-- eval (If b t1 t2)    --                                                                         Dados algébricos generalizado
--   | evalB b = t1
--   | otherwise = t2


-- Exemplos de GADTs

data Term a where
  Lit :: Int -> Term Int
  LitB :: Bool -> Term Bool 
  Succ :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If :: Term Bool -> Term a -> Term a -> Term a
  
deriving instance Show a => Show (Term a)

eval :: Term a -> a
eval (Lit i) = i
eval (LitB b) = b
eval (Succ i) = 1 + eval i
eval (IsZero b) = 0 == eval b
eval (If b t1 t2)
  | eval b = eval t1
  | otherwise = eval t2


data R
data B
data RBTree a c where
  Leaf :: RBTree a B
  Red :: a -> RBTree a B -> RBTree a B -> RBTree a R
  Black :: a -> RBTree a c -> RBTree a c -> RBTree a B

deriving instance Show a => Show (RBTree a c)


