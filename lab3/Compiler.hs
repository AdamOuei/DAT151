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


compileStm :: Stm -> Compile ()
compileStm stm = 
          case stm of
            case stm of 
              SExp exp -> do
                compileExp exp
                 
              SDecls typ ids -> do
                    emit "lol"
              SInit typ id exp -> do
                  compileExp
                  val <- lookupAddr id
                  emit $ "istore" ++ show addr
              SRet exp -> do
                  compileExp exp
                  emit "ireturn"
                  -- Newlabel maybe?
              SWhile exp stm' -> do
                   emit $ "TEST:"
                   compileExp exp
                   emit $ "ifeq END"
                   newBlock
                   compileStm stm'
                   exitBlock
                   emit $ "goto TEST"                
                   emit $ "END:"
              SIf exp stm1 stm2 -> do
                  false <- newLabel "FALSE"
                  true <- newLabel "TRUE"
                  compileExp exp
                  newBlock
                  emit $ "ifeq FALSE"
                  compileStm stm1
                  emit $ "goto TRUE"
                  emit $ "FALSE:"
                  compileStm stm2
                  emit $ "TRUE:"
                  exitBlock
              SBlock stmxs -> do
                    newBlock
                    mapM_ compileStm stmxs
                    exitBlock
                  



compileExp :: Stm -> State Env ()
compile stm = 
          case stm of
                    EInt int -> emit $ "ldc" ++ show i
                    EDouble doub -> emit $ "ldc2_w" ++ show i
                    ETrue -> emit "bipush 1"
                    EFalse -> emit "bipush 0"
                    EId id -> do
                            addr <- lookupAddr id
                            emit $ "iload" ++ show addr 
                    ECall id argExps -> do
                      emit "2"
                    EInc id -> do
                       addr <- lookupAddr id
                       emit $ "iload" ++ show addr
                       emit $ "iinc" ++ show addr ++ "1"
                    EDec id -> do
                      addr <- lookupAddr id
                      emit $ "iload" ++ show addr
                      emit $ "iinc" ++ show addr ++ "-1"
                    EInc2 id ->do
                      addr <- lookupAddr id
                      emit $ "iinc" ++ show addr ++ "1"
                      emit $ "iload" ++ show addr
                    EDec2 id ->do
                      addr <- lookupAddr id
                      emit $ "iinc" ++ show addr ++ "- 1"
                      emit $ "iload" ++ show addr
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
                        true <- newLabel "TRUE"
                        end <- newLabel "END"
                        emit $ "if_icmplt" ++ true
                        emit "bipush 0"
                        emit $ "goto " ++ end
                        emit $ true ++ ":"
                        emit "bipush 1"
                        emit $ end ++ ":"
                    EGre exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      true <- newLabel "TRUE"
                      end <- newLabel "END"
                      emit $ "if_icmpgt" ++ true
                      emit "bipush 0"
                      emit $ "goto " ++ end
                      emit $ true ++ ":"
                      emit "bipush 1"
                      emit $ end ++ ":"
                    ELeq exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      true <- newLabel "TRUE"
                      end <- newLabel "END"
                      emit $ "if_icmple" ++ true
                      emit "bipush 0"
                      emit $ "goto " ++ end
                      emit $ true ++ ":"
                      emit "bipush 1"
                      emit $ end ++ ":"
                    EGeq exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      true <- newLabel "TRUE"
                      end <- newLabel "END"
                      emit $ "if_icmpge" ++ true
                      emit "bipush 0"
                      emit $ "goto " ++ end
                      emit $ true ++ ":"
                      emit "bipush 1"
                      emit $ end ++ ":"
                    EEqua exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      true <- newLabel "TRUE"
                      end <- newLabel "END"
                      emit $ "if_icmpeq" ++ true
                      emit "bipush 0"
                      emit $ "goto " ++ end
                      emit $ true ++ ":"
                      emit "bipush 1"
                      emit $ end ++ ":"
                    EIneq exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      true <- newLabel "TRUE"
                      end <- newLabel "END"
                      emit $ "if_icmpne" ++ true
                      emit "bipush 0"
                      emit $ "goto " ++ end
                      emit $ true ++ ":"
                      emit "bipush 1"
                      emit $ end ++ ":"
                    EConj exp1 exp2 ->do
                      compileExp exp1
                      end <- newLabel "END"
                      emit "dup"
                      emit $ "ifeq" ++ end
                      emit "pop"
                      compileExp exp2
                      emit $ end ++ ":"
                    EDisj exp1 exp2 -> do
                      compileExp exp1
                      end <- newLabel "END"
                      emit "dup"
                      emit $ "ifne" ++ end
                      emit "pop"
                      compileExp exp2
                      emit $ end ++ ":"
                    EAss id exp -> do
                      compileExp exp
                      addr <- lookupAddr id
                      emit "dup"
                      emit $ "istore" ++ show addr


-- compileFun :: FDef -> Compile ()

emit :: Instruction -> State Env ()
emit i = emit' $ " " ++ i

emit' :: String -> State Env ()
emit' str = modify (\env -> env{code = str : code env})



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

