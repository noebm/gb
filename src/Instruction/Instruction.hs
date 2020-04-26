module Instruction.Instruction
  ( Expr (..)
  , ConditionalExpr (..)
  , Instruction (..)
  , Instruction'
  , expr
  , branch, flag
  , instructionASM
  )
where

import Instruction.Types.Address
import Instruction.Types.Flag (Flag)
import Instruction.Types.Expr

import Text.Printf
import Data.Word
import Data.Int

import Data.Functor.Contravariant

type Instruction' = Instruction Flag Word

data Instruction f a
  = InstructionNode a Expr
  | InstructionBranch a a f ConditionalExpr
  deriving Show

-- conditional instructions
data ConditionalExpr = JP' Addr | JR' !Int8 | CALL' !Word16 | RET'
  deriving Show

instance Functor (Instruction f) where
  fmap f (InstructionNode x e) = InstructionNode (f x) e
  fmap f (InstructionBranch x y flg e) = InstructionBranch (f x) (f y) flg e

exprASM' :: ConditionalExpr -> Flag -> String
exprASM' (JP' addr) flg = printf "JP %s,%s" (show flg) (show addr)
exprASM' (JR' addr) flg = printf "JR %s,%i" (show flg) (show addr)
exprASM' (CALL' addr) flg = printf "CALL %s,%s" (show flg) (show addr)
exprASM' RET' flg = printf "RET %s" (show flg)

instructionASM :: Instruction Flag a -> String
instructionASM (InstructionNode _ e) = exprASM e
instructionASM (InstructionBranch _ _ flg e) = exprASM' e flg

{-# INLINE flag #-}
flag :: Applicative g => (f -> g f') -> Instruction f a -> g (Instruction f' a)
flag _ (InstructionNode x e) = pure $ InstructionNode x e
flag f (InstructionBranch x y flg e) = (\flg' -> InstructionBranch x y flg' e) <$> f flg

{-# INLINE expr #-}
expr :: Contravariant g => (Expr -> g Expr) -> Instruction f a -> g (Instruction f a)
expr f = contramap expr' . f . expr' where
  expr' (InstructionNode _ op) = op
  expr' (InstructionBranch _ _ _ op) = case op of
    JP' addr -> JP addr
    JR' addr -> JR addr
    CALL' addr -> CALL addr
    RET' -> RET

{-# INLINE branch #-}
branch :: Functor g => (a -> g a) -> Instruction Bool a -> g (Instruction Bool a)
branch f (InstructionNode x op) = (\x' -> InstructionNode x' op) <$> f x
branch f (InstructionBranch x y flg op) = if flg
  then (\y' -> InstructionBranch x y' flg op) <$> f y
  else (\x' -> InstructionBranch x' y flg op) <$> f x
