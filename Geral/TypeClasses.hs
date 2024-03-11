data Temperature  = C Float | F Float
    deriving(Show)

instance Eq Temperature where
    (==) (C n) (C m) = n == m
    (==) (F n) (F m) = n == m
    (==) (F n) (C m) = (1.8*m + 32) == n
    (==) (C n) (F m) = (1.8*n + 32) == m


