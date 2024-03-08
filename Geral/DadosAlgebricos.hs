data Semana = Dom | Seg | Ter | Qua | Qui | Sex | Sab
  deriving (Show, Eq, Ord) -- Fala pro compilador gerar automaticamente uma definição da classe Show, Eq, Ord para o tipo de dado

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

data Lista a = a :> (Lista a) | Nil
  deriving (Show)

tamanho :: (Num b) => Lista a -> b
tamanho Nil = 0
tamanho (a :> l) = 1 + tamanho l

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

