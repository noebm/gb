module CPU.Registers where

data Reg8 = A | B | C | D | E | F | H | L
  deriving (Eq, Show)

data Reg16 = AF | BC | DE | HL
  deriving (Eq, Show)
