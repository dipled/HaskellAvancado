data Bin = V | Z Bin | U Bin

data SigBin = Neg Bin | Pos Bin

instance Show Bin where
  show V = ""
  show (Z n) = "0" ++ show n
  show (U n) = "1" ++ show n

instance Show SigBin where
  show (Neg n) = "-" ++ show n
  show (Pos n) = "+" ++ show n