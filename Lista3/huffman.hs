{-# LANGUAGE GADTSyntax #-}

data Huffman where
  Folha :: Int -> Char -> Huffman
  No :: Int -> Huffman -> Huffman -> Huffman
  deriving (Show)

instance Eq Huffman where
  (==) :: Huffman -> Huffman -> Bool
  a == b = pegaNumero a == pegaNumero b

instance Ord Huffman where
  (<=) :: Huffman -> Huffman -> Bool
  a <= b = pegaNumero a <= pegaNumero b



sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort l@(x : xs) = sort (filter (< x) xs) ++ filter (== x) l ++ sort (filter (> x) xs)

insert :: (Ord a) => a -> [a] -> [a]
insert a [] = [a]
insert a l@(x:xs)
    | a <= x = a : l
    | otherwise = x : insert a xs

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

freqSimb :: String -> [Huffman]
freqSimb [] = []
freqSimb s@(x : xs) = Folha (count x s) x : freqSimb (filter (x /=) xs)

pegaNumero :: Huffman -> Int
pegaNumero (Folha i c) = i
pegaNumero (No i l r) = i

construirArvore :: [Huffman] -> Huffman
construirArvore [] = error "Arvore Vazia"
construirArvore [x] = x
construirArvore (x : y : xs) = construirArvore $  insert (No (pegaNumero x + pegaNumero y) x y) xs

codHuffmanAux :: String -> Huffman -> [(Char, String)]
codHuffmanAux s (No i l r) = codHuffmanAux (s ++ "0") l ++ codHuffmanAux (s ++ "1") r
codHuffmanAux s (Folha i c) = [(c, s)]

codHuffman :: Huffman -> [(Char, String)]
codHuffman = codHuffmanAux ""


codificarAux :: String -> [(Char, String)] -> [String]
codificarAux [] _ = []
codificarAux s@(x:xs) huffmanCode = map snd (filter ((x ==) . fst) huffmanCode) ++ codificarAux xs huffmanCode

codificar :: String -> Huffman -> String
codificar "" _ = ""
codificar s@(x:xs) h = concat $ codificarAux s $ codHuffman h

decodificarCaractere :: String -> Huffman -> (Char, String)
decodificarCaractere s (Folha i c) = (c, s)
decodificarCaractere s@(x:xs) h@(No i l r)
    |x == '0' = decodificarCaractere xs l
    |x == '1' = decodificarCaractere xs r

decodificar :: String -> Huffman -> String
decodificar [] _ = []
decodificar s@(x:xs) h@(No i l r) = caractere : decodificar resto h where (caractere, resto) = decodificarCaractere s (No i l r)

arvore :: String -> Huffman
arvore s = construirArvore $ sort $ freqSimb $ s

codificaTotal :: String -> String
codificaTotal s = codificar s $ arvore s
