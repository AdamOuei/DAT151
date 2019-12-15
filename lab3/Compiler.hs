-- Optional: turn on warnings.
-- {-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Compiler for C--, producing symbolic JVM assembler.

module Compiler where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Annotated

 -- | Entry point.

data Env = Env {
  vars :: [Map Ident Int],
  maxvar :: Int,
  code :: [Instruction],
  labelCount :: Int
}

compile
  :: String  -- ^ Class name.
  -> Program -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
compile name _prg = header
  where
  header :: String
  header = unlines
    [ ";; BEGIN HEADER"
    , ""
    , ".class public " ++ name
    , ".super java/lang/Object"
    , ""
    , ".method public <init>()V"
    , "  .limit locals 1"
    , ""
    , "  aload_0"
    , "  invokespecial java/lang/Object/<init>()V"
    , "  return"
    , ""
    , ".end method"
    , ""
    , ".method public static main([Ljava/lang/String;)V"
    , "  .limit locals 1"
    , "  .limit stack  1"
    , ""
    , "  invokestatic " ++ name ++ "/main()I"
    , "  pop"
    , "  return"
    , ""
    , ".end method"
    , ""
    , ";; END HEADER"
    ]


--compileStm :: Exp -> Compile ()


compileExp :: Stm -> State Env ()
compile stm = 
          case stm of
                    EInt int -> emit $ "ldc" ++ show i
                    EDouble doub -> emit $ "ldc" ++ show i
                    ETrue -> emit "bipush 1"
                    EFalse -> emit "bipush 0"
                    EId id -> do
                            val <- lookupVar id
                            emit $ "iload" ++ show val 
                    ECall id argExps -> do
                      emit "2"
                    EInc id -> do
                       val <- lookupVar id
                       emit $ "iload" ++ show val
                       emit $ "iinc" ++ show val ++ "1"
                    EDec id -> do
                      val <- lookupVar id
                      emit $ "iload" ++ show val
                      emit $ "iinc" ++ show val ++ "-1"
                    EInc2 id ->do
                      val <- lookupVar id
                      emit $ "iinc" ++ show val ++ "1"
                      emit $ "iload" ++ show val
                    EDec2 id ->do
                      val <- lookupVar id
                      emit $ "iinc" ++ show val ++ "- 1"
                      emit $ "iload" ++ show val
                    EMul exp1 exp2 -> do
                        compileExp exp1
                        compileExp exp2
                        emit "imul"
                    EDiv exp1 exp2 ->  do
                      compileExp exp1
                      compileExp exp2
                      emit "idiv"
                    EAdd exp1 exp2 ->  do
                      compileExp exp1
                      compileExp exp2
                      emit "iadd"
                    ESub exp1 exp2 ->  do
                      compileExp exp1
                      compileExp exp2
                      emit "isub"
                    ELess exp1 exp2 ->
                      do
                        compileExp exp1
                        compileExp exp2
                        emit "if_icmplt"
                    EGre exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      emit "if_icmpgt"
                    ELeq exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      emit "if_icmple"
                    EGeq exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      emit "if_icmpge"
                    EEqua exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      emit "if_icmpeq"
                    EIneq exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      emit "if_icmpne"
                    EConj exp1 exp2 ->do
                      compileExp exp1
                      compileExp exp2
                      emit "ifeq"
                    EDisj exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      emit "ifne"
                    EAss id exp -> do
                      compileExp exp
                      val <- lookupVar id
                      emit "dup"
                      emit $ "istore" ++ show val


-- compileFun :: FDef -> Compile ()

-- emit :: Code -> Compile ()

lookupAddr :: Id -> State Env Int
lookupAddr id = gets $ helper . vars
            where helper (ctx:stack) = fromMaybe (helper stack) (Map.lookup id ctx)

-- lookupFun :: FDef -> Type

-- extendId :: Id -> Type -> Compile ()

-- extendFunc :: FDef -> Compile ()

newBlock :: State Env ()
newBlock = modify (\env -> env{vars = Map.empoty:vars env})

-- exitBlock :: Compile ()

emptyEnv ::  Env
emptyEnv =  Env{
            vars = [Map.empty],
            maxvar = 0,
            code = []
}
newLabel :: String -> State Env Label
newLabel labelName = do
    env <- get
    modify (\env -> env {labelCount = 1 + labelCount env})
    return $ labelName ++ show (labelCount env)

