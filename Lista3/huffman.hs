{-# LANGUAGE GADTSyntax #-}

import Data.Binary.Get qualified as G
import Data.Binary.Put qualified as P
import Data.Bits
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
freqSimb s = ordena $ go s
  where
    go [] = []
    go s@(x : xs) = Folha (count x s) x : go (filter (x /=) xs)

pegaNumero :: Huffman -> Int
pegaNumero (Folha i c) = i
pegaNumero (No i l r) = i

construirArvore :: [Huffman] -> Huffman
construirArvore [] = error "Arvore Vazia"
construirArvore [x] = x
construirArvore (x : y : xs) = construirArvore $ insere (No (pegaNumero x + pegaNumero y) x y) xs

codHuffman :: Huffman -> [(Char, String)]
codHuffman = go ""
  where
    go s (No i l r) = go (s ++ "0") l ++ go (s ++ "1") r
    go s (Folha i c) = [(c, s)]

codificar :: String -> Huffman -> String
codificar "" _ = ""
codificar s@(x : xs) h = concat $ go s $ codHuffman h
  where
    go [] _ = []
    go s@(x : xs) huffmanCode = map snd (filter ((x ==) . fst) huffmanCode) ++ go xs huffmanCode

decodificar :: String -> Huffman -> String
decodificar [] _ = []
decodificar s@(x : xs) h@(No i l r) = let (caractere, resto) = go s (No i l r) in caractere : decodificar resto h
  where
    go s (Folha i c) = (c, s)
    go s@(x : xs) h@(No i l r)
      | x == '0' = go xs l
      | x == '1' = go xs r

freq :: (Eq a) => [a] -> [(a, Int)]
freq [] = []
freq (x : xs) = let (l1, l2) = partition (== x) xs in (x, length l1 + 1) : freq l2

putStart :: (Int, Int) -> P.Put
putStart (n, t) =
  do
    P.putWord8 $ toEnum n
    P.putWord32be $ toEnum t
    P.flush

putFreqList :: [(Char, Int)] -> P.Put
putFreqList [] = P.flush
putFreqList ((c, f) : xs) =
  do
    P.putWord8 $ I.c2w c
    P.putWord32be $ toEnum f
    putFreqList xs

s2b :: String -> Word8
s2b string = toEnum $ go string
  where
    go n = goAux n (length n - 1)
      where
        goAux "" len = 0
        goAux "1" len = 1
        goAux "0" len = 0
        goAux ('0' : xs) len = 0 + goAux xs (len - 1)
        goAux ('1' : xs) len = (2 ^ len) + goAux xs (len - 1)
        goAux _ len = error "Illegal character"

b2s :: Word8 -> [Char]
b2s n = if (length $ go n) >= 8 then go n else (completaZeros $ 8 - (length $ go n)) $ go n
  where
    go n
      | mod n 2 == 0 = if n /= 0 then (go $ div n 2) ++ "0" else ""
      | otherwise = (go $ div n 2) ++ "1"

completaZeros :: Int -> String -> String
completaZeros 0 s = s
completaZeros n s = '0' : completaZeros (n - 1) s

putFullTxt :: String -> P.Put
putFullTxt [] = P.flush
putFullTxt s =
  do
    P.putWord8 $ s2b $ take 8 s
    putFullTxt $ drop 8 s

-- escrita :: IO ()
-- escrita =
--   do
--     txt <- readFile "file.txt"
--     let xs = freq txt
--     let bs = P.runPut $ putFreqList xs
--     putStrLn $ show xs
--     L.writeFile "file.bin" $  bs

getReg :: G.Get Reg
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
    putStrLn $ (show $ I.w2c c) ++ " - " ++ show f
    printRegs rs

printStart :: Reg -> IO ()
printStart (n, t) =
  do
    putStrLn $ show n ++ " - " ++ show t

getC :: G.Get Word8
getC = G.getWord8 >>= \x -> return x

getMsg :: G.Get [Word8] -- argumento da funcao propagado pelo pipeline da monada
getMsg =
  do
    empty <- G.isEmpty
    if empty
      then return []
      else do x <- getC; xs <- getMsg; return (x : xs)

printMsg :: [Word8] -> IO ()
printMsg [] = return ()
printMsg (x : xs) = (putStrLn $ show x) >> printMsg xs

-- leitura :: IO ()
-- leitura =
--   do
--     bs <- L.readFile "file.bin"
--     let rs = G.runGet getRegs bs
--     printRegs rs

reg2LeafList :: [Reg] -> [Huffman]
reg2LeafList r = ordena $ go r
  where
    go [] = []
    go ((c, f) : rs) = (Folha (read $ show f) $ I.w2c c) : go rs

escrita :: IO ()
escrita =
  do
    txt <- readFile "file.txt"
    let xs = freq txt
    let fr = freqSimb txt
    let tr = construirArvore fr
    let hf = codHuffman tr
    let final = codificar txt tr
    let uniqueCharacters = length xs
    let totalCharacters = sum $ map snd xs
    let bs1 = P.runPut $ putStart (uniqueCharacters, totalCharacters)
    let bs = P.runPut $ putFreqList xs
    let bs2 = P.runPut $ putFullTxt final
    putStrLn $ show xs
    L.writeFile "file.bin" $ bs1 <> bs <> bs2

leitura :: IO ()
leitura =
  do
    bs <- L.readFile "file.bin"
    let regHead@(n, t) = G.runGet getReg bs
    let regTail = G.runGet getRegs $ L.take ((read $ show n) * 5) $ L.drop 5 bs
    let msg = G.runGet getMsg $ L.drop ((read $ show n) * 5 + 5) bs
    printStart regHead
    printRegs regTail
    let fr = reg2LeafList regTail
    print msg
    let binMsg = concat $ map b2s msg
    let msgDecodificada = decodificar binMsg $ construirArvore fr
    print binMsg
    print fr
    print $ codHuffman $ construirArvore fr
    print msgDecodificada

passoAPasso :: IO () =
  do
    putStrLn "Digite uma String"
    ln <- getLine
    let fr = freqSimb ln
    print fr
    putStrLn ""
    let tr = construirArvore fr
    print tr
    putStrLn ""
    let hf = codHuffman tr
    print hf
    putStrLn ""
    let final = codificar ln tr
    print $ "codificado: " ++ final
    print $ s2b $ take 8 final
    putStrLn ""
    putStrLn ""
    print $ "decodificado: " ++ decodificar final tr