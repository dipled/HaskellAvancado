fix f = let x = f x in x

fat n = fix (\f x -> if x == 0 then 1 else x * f (x - 1)) n


zero::(a -> a) -> a -> a
zero = \s z -> z
um::(a -> a) -> a -> a
um = \s z -> s z
dois = \s z -> s (s z)
tres = \s z -> s (s (s z))
quatro = \s z -> s (s (s (s z)))


suc = \w y x -> y (w y x)
prede = \n f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u)
add = \x y w u -> x w (y w u)
mul = \x y w u -> x (y w) u

v = \v f -> v
f = \v f -> f

e = \x y -> x y (\u v -> v)
ou = \x y -> x (\u v -> u) y
nao = \x -> x (\u v -> v) (\a b -> a)

ehZero = \n -> n (\d -> f) v

teste = \n -> (ehZero n) um dois

