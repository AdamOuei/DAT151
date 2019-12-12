{-# LANGUAGE LambdaCase #-}

-- | Typed syntax for C--.

module Annotated where

import qualified CMM.Abs as A

-- This is a stub for typed ASTs produced by the type checker
-- as input for the compiler.

-- To make the stub compile, we just define an alias to
-- untyped ASTs here.

data Program = PDefs [Func]
  deriving (Eq, Ord, Show, Read)

data Func = DFun Type Id [Arg] [Stm]
  deriving (Eq, Ord, Show, Read)

data Stm 
    = SExp Type Exp
    | SDecls Type [Id]
    | SInit Type Id Exp
    | SRet Type Exp
    | SWhile Exp Stm
    | SIf Exp Stm Stm
    | SBlock [Stm]
  deriving (Eq, Ord, Show, Read)
