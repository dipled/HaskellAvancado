{-# language FunctionalDependencies, GADTSyntax #-}

{-

  Dadas essas funcoes aki:
    read :: String -> a
    show :: a -> String

  Se tentarmos definir essa funcao aki
    foo s = show (read a)
  Haskell acusaria de tipagem ambígua


-}

data TermI = Lit Int | Succ TermI
    deriving(Show)

data TermB = LitB Bool | IsZero TermI
    deriving(Show)

data Term = If TermB Term Term | TB TermB | TI TermI
    deriving(Show)

class Eval a b | a -> b where  -- essa dependência indica que a partir do tipo a, é possível
                               -- determinar o tipo b
    eval :: a -> b



data Temperature  = C Float | F Float
    deriving(Show)

instance Eq Temperature where
    (==) (C n) (C m) = n == m
    (==) (F n) (F m) = n == m
    (==) (F n) (C m) = (1.8*m + 32) == n
    (==) (C n) (F m) = (1.8*n + 32) == m


