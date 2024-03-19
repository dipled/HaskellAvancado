data Bin = V | Z Bin | U Bin

data SigBin = Neg Bin | Pos Bin

instance Show Bin where
  show V = ""
  show (Z n) = "0" ++ show n
  show (U n) = "1" ++ show n

instance Show SigBin where
  show (Neg V) = ""
  show (Pos V) = ""
  show (Neg n) = "-" ++ show n
  show (Pos n) = "+" ++ show n

instance Num SigBin where
  a + b = integerToBin ((sigBinToInteger a) + (sigBinToInteger b))
  a - b = integerToBin ((sigBinToInteger a) - (sigBinToInteger b))
  a * b = integerToBin ((sigBinToInteger a) * (sigBinToInteger b))
  abs (Neg a) = Pos a
  abs (Pos a) = Pos a

  signum (Neg V) = (Pos V)
  signum (Pos V) = (Pos V)
  signum (Neg (Z V)) = (Pos (Z V))
  signum (Pos (Z V)) = (Pos (Z V))
  signum (Neg a) = (Neg (U V))
  signum (Pos a) = (Pos (U V))

  fromInteger a = integerToBin a

binConcat :: Bin -> Bin -> Bin
binConcat b1 V = V
binConcat V b2 = b2
binConcat (Z b1) b2 = Z (binConcat b1 b2)
binConcat (U b1) b2 = U (binConcat b1 b2)

sigBinToInteger :: SigBin -> Integer
sigBinToInteger (Pos n) = binToInteger n
sigBinToInteger (Neg n) = binToInteger n * (-1)

binToInteger :: Bin -> Integer
binToInteger n = binToIntegerAux n ((length (show n)) - 1)

binToIntegerAux :: Bin -> Int -> Integer
binToIntegerAux V len = 0
binToIntegerAux (Z n) len = 0 + binToIntegerAux n (len -1)
binToIntegerAux (U n) len = (2^len) + binToIntegerAux n (len-1)

integerToBin :: Integer -> SigBin
integerToBin n 
  | n > 0 = Pos  (integerToBinAux n)
  | n < 0 = Neg (integerToBinAux (-n))
  | otherwise = Pos (Z V)

integerToBinAux :: Integer -> Bin
integerToBinAux n 
  | mod n 2 == 0 = if n /= 0 then binConcat (integerToBinAux (div n 2)) (Z V)  else V
  | otherwise = binConcat (integerToBinAux (div n 2)) (U V)