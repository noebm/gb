module CPU.Registers where

data Reg8 = A | B | C | D | E | F | H | L
  deriving (Eq, Show)

data Reg16 = AF | BC | DE | HL
  deriving (Eq, Show)

regPair :: Reg16 -> (Reg8, Reg8)
regPair AF = (F , A)
regPair BC = (C , B)
regPair DE = (E , D)
regPair HL = (L , H)
