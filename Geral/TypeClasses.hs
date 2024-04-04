{-# LANGUAGE FunctionalDependencies, GADTSyntax #-}

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

data Res = RI Int | RB Bool

class Eval a b | a -> b where  -- essa dependência indica que a partir do tipo a, é possível
                               -- determinar o tipo b
    eval :: a -> b

instance Num (Maybe Int) where
  (+) :: Maybe Int -> Maybe Int -> Maybe Int
  Nothing + _ = Nothing
  _ + Nothing = Nothing
  (Just a) + (Just b) = Just (a + b)

  (-) :: Maybe Int -> Maybe Int -> Maybe Int
  Nothing - _ = Nothing
  _ - Nothing = Nothing
  (Just a) - (Just b) = Just (a - b)

  (*) :: Maybe Int -> Maybe Int -> Maybe Int
  Nothing * _ = Nothing
  _ * Nothing = Nothing
  (Just a) * (Just b) = Just (a * b)

  abs :: Maybe Int -> Maybe Int
  abs Nothing = Nothing
  abs (Just a)
    | a >= 0 = Just a
    | otherwise = Just a * (-1)

  signum :: Maybe Int -> Maybe Int 
  signum Nothing = Nothing
  signum (Just a)
    | a > 0 = Just 1
    | a == 0 = Just 0
    | otherwise = Just (-1)

  fromInteger :: Integer -> Maybe Int
  fromInteger a = Just (fromInteger a)

(!) :: Maybe Int -> Maybe Int -> Maybe Int
Nothing ! _ = Nothing
_ ! Nothing = Nothing
(Just a) ! (Just b)
  | b == 0 = Nothing
  | otherwise = Just (quot a b)

(%) :: Maybe Int -> Maybe Int -> Maybe Int

Nothing % _ = Nothing
_ % Nothing = Nothing

(Just a) % (Just b)
  | b == 0 = Nothing
  | otherwise = Just (a `rem` b)


instance Eval TermI Int where
  eval (Lit i) = i
  eval (Succ t) = 1 + eval t

instance Eval TermB Bool where
  eval (LitB b) = b
  eval (IsZero t) = 0 == eval t

instance Eval Term Res where
  eval (If b t1 t2) = if eval b then eval t1 else eval t2
  eval (TB t) = RB (eval t)
  eval (TI t) = RI (eval t)

data Temperature  where
   C :: Float -> Temperature  
   F :: Float -> Temperature
    deriving(Show)

instance Eq Temperature where
    (==) (C n) (C m) = n == m
    (==) (F n) (F m) = n == m
    (==) (F n) (C m) = (1.8*m + 32) == n
    (==) (C n) (F m) = (1.8*n + 32) == m


