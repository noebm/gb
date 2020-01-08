module CPU.Registers where

data Reg8 = A | B | C | D | E | F | H | L
  deriving (Eq, Show)

data Reg16 = AF | BC | DE | HL
  deriving (Eq, Show)

regPair :: Reg16 -> (Reg8, Reg8)
regPair AF = (A , F)
regPair BC = (B , C)
regPair DE = (D , E)
regPair HL = (H , L)
