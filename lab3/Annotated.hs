{-# LANGUAGE LambdaCase #-}

-- | Typed syntax for C--.

module Annotated where

import qualified CMM.Abs (Id(..), Type(..), Arg(..))

-- This is a stub for typed ASTs produced by the type checker
-- as input for the compiler.

-- To make the stub compile, we just define an alias to
-- untyped ASTs here.

data Program = PDefs [Def]
  deriving (Eq, Ord, Show, Read)

data Def = DFun Type Id [Arg] [Stm]
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

data Exp 
    = EInt Integer
    | EDouble Double
    | ETrue
    | EFalse
    | EId Id
    | ECall Id [Exp]
    | EInc Id
    | EDec Id
    | EInc2 Id
    | EDec2 Id
    | EMul Exp Exp
    | EDiv Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | ELess Exp Exp
    | EGre Exp Exp
    | ELeq Exp Exp
    | EGeq Exp Exp
    | EEqua Exp Exp
    | EIneq Exp Exp
    | EConj Exp Exp
    | EDisj Exp Exp
    | EAss Id Exp
    | ETyped Exp Type
  deriving (Eq, Ord, Show, Read)


