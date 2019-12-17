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

import CMM.Abs (Id(..), Type(..), Args(..))
import TypeChecker (FunType (..))

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Annotated

 -- | Entry point.

data Env = Env {
  vars :: [Map Id (Int, Type)],
  maxvar :: Int,
  code :: [Instruction],
  labelCount :: Int,
  funs :: Map Id FunString, 
  className :: String}

data Fun = Fun { funId :: Id, funFunType :: FunType } 

type Instruction = String 
type Label = String
type FunString = String

compile
  :: String  -- ^ Class name.
  -> Program -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
compile name (PDefs funs) = header ++ concatMap compileFuns funs
  where
      makeEnv :: Env
      makeEnv = foldl extendFunc (emptyEnv name) funs
      compileFuns :: Func -> String
      compileFuns fun = unlines . reverse . code $ execState  (compileFun fun) makeEnv

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


compileStm :: Stm -> State Env ()
compileStm stm = 
          case stm of
              SExp typ exp -> do
                compileExp exp
                noPop <- isVoidFunApp exp
                unless noPop $ emit "pop"
              SDecls typ ids -> mapM_ (extendId typ) ids
              SInit typ id exp -> do
                  compileExp exp
                  addr <- extendId typ id
                  case typ of
                    TDouble -> emit $ "dstore " ++ show addr
                    _ -> emit $ "istore " ++ show addr
              SRet typ exp -> do
                  compileExp exp
                  case typ of
                    TDouble -> emit "dreturn"
                    _ -> emit "ireturn"
                  -- Newlabel maybe?
              SWhile exp stm' -> do
                   test <- newLabel "TEST"
                   end <- newLabel "END"
                   emit $ test ++ ":"
                   compileExp exp
                   emit $ "ifeq " ++ end
                   newBlock
                   compileStm stm'
                   exitBlock
                   emit $ "goto " ++ test               
                   emit $ end ++ ":"
              SIf exp stm1 stm2 -> do
                  false <- newLabel "FALSE"
                  true <- newLabel "TRUE"
                  compileExp exp
                  newBlock
                  emit $ "ifeq " ++ false
                  compileStm stm1
                  emit $ "goto " ++ true
                  emit $ false ++ ":"
                  compileStm stm2
                  emit $ true ++ ":"
                  exitBlock
              SBlock stmxs -> do
                    newBlock
                    mapM_ compileStm stmxs
                    exitBlock
              where
                  isVoidFunApp :: Exp -> State Env Bool
                  isVoidFunApp (ECall id _) = isVoidFun id
                  isVoidFunApp _ = return False
              
                  isVoidFun :: Id -> State Env Bool
                  isVoidFun id = do
                    jvmFunType <- lookupFun id
                    return $ last jvmFunType == 'V'
                  



compileExp :: Exp -> State Env ()
compileExp exp = 
          case exp of
                    EInt int -> emit $ "ldc " ++ show int
                    EDouble doub -> emit $ "ldc2_w " ++ show doub
                    ETrue -> emit "bipush 1 "
                    EFalse -> emit "bipush 0 "
                    EId id -> do
                            addr <- lookupAddr id
                            case lookupType id of
                              TDouble -> emit $ "dload " ++ show addr
                              _ -> emit $ "iload " ++ show addr 
                    ECall id argExps -> do
                      mapM_ compileExp argExps
                      sig <- getSig id -- Testa ask
                      emit $ "invokestatic " ++ sig
                    EInc id -> do
                       addr <- lookupAddr id
                       case lookupType id of
                        TDouble -> do 
                              emit $ "dload " ++ show addr
                              emit $ "dinc " ++ show addr ++ " 1"
                        _ -> do
                              emit $ "iload " ++ show addr
                              emit $ "iinc " ++ show addr ++ " 1"
                    EDec id -> do
                      addr <- lookupAddr id
                      case lookupType id of
                        TDouble -> do 
                              emit $ "dload " ++ show addr
                              emit $ "dinc " ++ show addr ++ " 1"
                        _ -> do
                              emit $ "iload " ++ show addr
                              emit $ "iinc " ++ show addr ++ " 1"
                    EInc2 id ->do
                      addr <- lookupAddr id
                      case lookupType id of
                        TDouble -> do 
                              emit $ "dload " ++ show addr
                              emit $ "dinc " ++ show addr ++ " 1"
                        _ -> do
                              emit $ "iload " ++ show addr
                              emit $ "iinc " ++ show addr ++ " 1"
                    EDec2 id ->do
                      addr <- lookupAddr id
                      case lookupType id of
                        TDouble -> do 
                              emit $ "dload " ++ show addr
                              emit $ "dinc " ++ show addr ++ " 1"
                        _ -> do
                              emit $ "iload " ++ show addr
                              emit $ "iinc " ++ show addr ++ " 1"
                    EMul exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      case getType (EMul exp1 exp2) of
                        TDouble -> emit "dmul "
                        _ -> emit "imul "
                    EDiv exp1 exp2 ->  do
                      compileExp exp1
                      compileExp exp2
                      case getType (EDiv exp1 exp2) of
                        TDouble -> emit "ddiv "
                        _ -> emit "idiv "
                    EAdd exp1 exp2 ->  do
                      compileExp exp1
                      compileExp exp2
                      case getType (EAdd exp1 exp2) of
                        TDouble -> emit "dadd "
                        _ -> emit "iadd "
                    ESub exp1 exp2 ->  do
                      compileExp exp1
                      compileExp exp2
                      case getType (ESub exp1 exp2) of 
                        TDouble -> emit "dsub "
                        _ -> emit "isub "
                    ELess exp1 exp2 ->
                      do
                        compileExp exp1
                        compileExp exp2
                        true <- newLabel "TRUE"
                        end <- newLabel "END"
                        emit $ "if_icmplt " ++ true
                        emit "bipush 0 "
                        emit $ "goto " ++ end
                        emit $ true ++ ":"
                        emit "bipush 1"
                        emit $ end ++ ":"
                    EGre exp1 exp2 -> do
                      compileExp exp1
                      compileExp exp2
                      true <- newLabel "TRUE"
                      end <- newLabel "END"
                      emit $ "if_icmpgt " ++ true
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
                      emit $ "if_icmple " ++ true
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
                      emit $ "if_icmpge " ++ true
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
                      emit $ "if_icmpeq " ++ true
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
                      emit $ "if_icmpne " ++ true
                      emit "bipush 0"
                      emit $ "goto " ++ end
                      emit $ true ++ ":"
                      emit "bipush 1"
                      emit $ end ++ ":"
                    EConj exp1 exp2 ->do
                      compileExp exp1
                      end <- newLabel "END"
                      emit "dup "
                      emit $ "ifeq " ++ end
                      emit "pop "
                      compileExp exp2
                      emit $ end ++ ":"
                    EDisj exp1 exp2 -> do
                      compileExp exp1
                      end <- newLabel "END"
                      emit "dup "
                      emit $ "ifne " ++ end
                      emit "pop "
                      compileExp exp2
                      emit $ end ++ ":"
                    EAss id exp -> do
                      compileExp exp
                      addr <- lookupAddr id
                      emit "dup "
                      case lookupType id of
                        TDouble -> emit $ "dstore " ++ show addr
                        _ -> emit $ "istore " ++ show addr


compileFun :: Func -> State Env ()
compileFun (DFun typ id args body) = do
            funString <- lookupFun id
            emit $ ".method public static " ++ funString
            emit ".limit locals 1000"
            emit ".limit stack 1000"
            mapM_  (\(FArgs typ id) -> extendId typ id) args 
            mapM_ compileStm body
            when (typ == TVoid) $ emit "return"
            emit' ".end method"
            emit ""

emit :: Instruction -> State Env ()
emit i = emit' $ " " ++ i

emit' :: String -> State Env ()
emit' str = modify (\env -> env{code = str : code env})

-- Kan vara denna som är seg som fan
getSig :: Id -> State Env FunString
getSig id = do
        env <- get
        let name = if isBuiltIn id
                    then "Runtime"
                    else className env
        funName <- lookupFun id
        return $ name ++ "/" ++ funName
          where isBuiltIn (Id id) = id `elem` ["printInt", "printDouble", "readInt", "readDouble"]



lookupAddr :: Id -> State Env Int
lookupAddr id = gets $ lookupAddr' . vars
            where lookupAddr' (ctx:stack) = fst $ fromMaybe (lookupAddr' stack) (Map.lookup id (ctx))
                  

lookupFun :: Id -> State Env FunString
lookupFun id = case id of
              (Id "printInt") -> return "printInt(I)V"
              (Id "printDouble") -> return "printDouble(D)V"
              (Id "readInt") -> return "readInt()I"
              (Id "readDouble") -> return "readDouble()D"
              id -> gets $ fromJust . Map.lookup id . funs

lookupType :: Id -> Type
lookupType id = gets $ snd . fromJust . Map.lookup id . vars
              
extendId :: Type -> Id -> State Env Int
extendId typ id = do
    env <- get
    let (ctx:ctxs) = vars env
    let varPos = maxvar env
    modify (\env -> env{vars = (Map.insert id (varPos, typ) ctx):ctxs, maxvar = varPos + 1})
    return varPos

extendFunc :: Env -> Func -> Env
extendFunc env func@(DFun typ id _ _) = env{funs = Map.insert id (rewriteFunc func) (funs env)} 

rewriteFunc :: Func -> FunString
rewriteFunc func@(DFun typ (Id name) args body) = 
                name ++ "(" ++ map getArgType args ++ ")" ++ [funType typ]
                where 
                    funType :: Type -> Char
                    funType typ = case typ of 
                                TBool -> 'Z'
                                TVoid -> 'V'
                                TInt -> 'I'
                                TDouble -> 'D'
                    getArgType :: Args -> Char
                    getArgType (FArgs typ _)= funType typ
                                    


newBlock :: State Env ()
newBlock = modify (\env -> env{vars = Map.empty:vars env})

exitBlock ::  State Env ()
exitBlock = do
  env <- get 
  let (ctx:ctxs) = vars env
  let newMaxVar = maxvar env - Map.size ctx 
  modify (\env -> env{vars = ctxs})


emptyEnv :: String -> Env
emptyEnv className =  Env{
            vars = [Map.empty],
            maxvar = 0,
            code = [],
            labelCount = 0,
            funs = Map.empty,
            className = className
}
newLabel :: String -> State Env Label
newLabel labelName = do
    env <- get
    modify (\env -> env {labelCount = 1 + labelCount env})
    return $ labelName ++ show (labelCount env)

getType :: Exp -> Type
getType exp = case exp of
  -- EInt int -> TInt
  -- EDouble doub -> TDouble
  -- ETrue -> TBool
  -- EFalse -> TBool
  -- EId id -> lookupType id
  -- ECall id exps -> 
  -- EInc id -> lookupType id
  -- EDec id -> lookupType id
  -- EInc2 id -> lookupType id
  -- EDec2 id -> lookupType id
  EMul exp1 exp2 -> inferBin exp1 exp2
  EDiv exp1 exp2 -> inferBin exp1 exp2
  EAdd exp1 exp2 -> inferBin exp1 exp2
  ESub exp1 exp2 -> inferBin exp1 exp2
  -- ELess exp1 exp2 -> TBool
  -- EGre exp1 exp2 -> TBool
  -- ELeq exp1 exp2 -> TBool
  -- EGeq exp1 exp2 -> TBool
  -- EEqua exp1 exp2 -> TBool
  -- EIneq exp1 exp2 -> TBool
  -- EConj exp1 exp2 -> TBool
  -- EDisj exp1 exp2 -> TBool
  -- EAss id exp -> lookupType id

inferBin :: Exp -> Exp -> Type
inferBin exp1 exp2 = case getType exp1 of
                            (TInt,_) -> case getType exp2 of
                                (TInt,_) -> TInt
                                (TDouble,_) -> TDouble
                            (TDouble, _) -> case getType exp2 of
                                (TInt, _) -> TDouble
                                (TDouble,_) -> TDouble
