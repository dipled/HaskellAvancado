{-# LANGUAGE GADTSyntax #-}

import Data.Bits
import Data.Binary.Get qualified as G
import Data.Binary.Put qualified as P
import Data.ByteString.Internal qualified as I
import Data.ByteString.Lazy qualified as L
import Data.List
import Data.Word

type Reg = (Word8, Word32)

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

ordena :: (Ord a) => [a] -> [a]
ordena [] = []
ordena [x] = [x]
ordena l@(x : xs) = ordena (filter (< x) xs) ++ filter (== x) l ++ ordena (filter (> x) xs)

insere :: (Ord a) => a -> [a] -> [a]
insere a [] = [a]
insere a l@(x : xs)
  | a <= x = a : l
  | otherwise = x : insere a xs

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
construirArvore (x : y : xs) = construirArvore $ insere (No (pegaNumero x + pegaNumero y) x y) xs

codHuffmanAux :: String -> Huffman -> [(Char, String)]
codHuffmanAux s (No i l r) = codHuffmanAux (s ++ "0") l ++ codHuffmanAux (s ++ "1") r
codHuffmanAux s (Folha i c) = [(c, s)]

codHuffman :: Huffman -> [(Char, String)]
codHuffman = codHuffmanAux ""

codificarAux :: String -> [(Char, String)] -> [String]
codificarAux [] _ = []
codificarAux s@(x : xs) huffmanCode = map snd (filter ((x ==) . fst) huffmanCode) ++ codificarAux xs huffmanCode

codificar :: String -> Huffman -> String
codificar "" _ = ""
codificar s@(x : xs) h = concat $ codificarAux s $ codHuffman h

decodificarCaractere :: String -> Huffman -> (Char, String)
decodificarCaractere s (Folha i c) = (c, s)
decodificarCaractere s@(x : xs) h@(No i l r)
  | x == '0' = decodificarCaractere xs l
  | x == '1' = decodificarCaractere xs r

decodificar :: String -> Huffman -> String
decodificar [] _ = []
decodificar s@(x : xs) h@(No i l r) = caractere : decodificar resto h where (caractere, resto) = decodificarCaractere s (No i l r)

arvore :: String -> Huffman
arvore s = construirArvore $ ordena $ freqSimb $ s

codificaTotal :: String -> String
codificaTotal s = codificar s $ arvore s

freq :: (Eq a) => [a] -> [(a, Int)]
freq [] = []
freq (x : xs) = let (l1, l2) = partition (== x) xs in (x, length l1 + 1) : freq l2

put :: [(Char, Int)] -> P.Put
put [] = P.flush
put ((c, f) : xs) =
  do
    P.putWord8 $ I.c2w c
    P.putWord32be $ toEnum f
    put xs

escrita :: IO ()
escrita =
  do
    txt <- readFile "file.txt"
    let xs = freq txt
    let bs = P.runPut $ put xs
    putStrLn $ show xs
    L.writeFile "file.bin" bs

getReg :: G.Get (Word8, Word32)
getReg =
  do
    c <- G.getWord8
    f <- G.getWord32be
    return (c, f)

getRegs :: G.Get [Reg] -- argumento da funcao propagado pelo pipeline da monada
getRegs =
  do
    empty <- G.isEmpty
    if empty
      then return []
      else do r <- getReg; rs <- getRegs; return (r : rs)

printRegs :: [Reg] -> IO ()
printRegs [] = return ()
printRegs ((c, f) : rs) =
  do
    putStrLn $ show (I.w2c c) ++ "-" ++ show f
    printRegs rs

leitura :: IO ()
leitura =
  do
    bs <- L.readFile "file.bin"
    let rs = G.runGet getRegs bs
    printRegs rs

-- s2b :: String -> Word8 -- assumindo que a String tem tamanho 8
-- s2b (x : xs) = 