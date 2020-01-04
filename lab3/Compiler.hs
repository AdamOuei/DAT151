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
  vars :: [Map Id Int],
  maxvar :: Int,
  code :: [Instruction],
  labelCount :: Int,
  funs :: Map Id FunString,
  className :: String
}


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
                compileExp typ exp
                case typ of
                  TDouble -> emit "pop2"
                  TVoid -> return ()
                  _ -> emit "pop"
              SDecls typ ids -> mapM_ (extendId typ) ids
              SInit typ id exp -> do
                  compileExp typ exp
                  addr <- extendId typ id
                  case typ of
                   TDouble -> emit $ "dstore " ++ show addr
                   _ -> emit $ "istore " ++ show addr
              SRet typ exp -> do
                  compileExp typ exp
                  case typ of
                    TDouble -> emit "dreturn"
                    TVoid -> return ()
                    _ -> emit "ireturn"
              SWhile exp stm' -> do
                   test <- newLabel "TEST"
                   end <- newLabel "END"
                   emit $ test ++ ":"
                   compileExp' exp
                   emit $ "ifeq " ++ end
                   newBlock
                   compileStm stm'
                   exitBlock
                   emit $ "goto " ++ test               
                   emit $ end ++ ":"
              SIf exp stm1 stm2 -> do
                  false <- newLabel "FALSE"
                  true <- newLabel "TRUE"
                  compileExp' exp
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

compileExp :: Type -> Exp -> State Env ()
compileExp TDouble exp@(ETyped exp' TInt) = compileExp' exp >> emit "i2d "
compileExp _ exp = compileExp' exp


compileExp' :: Exp -> State Env ()
compileExp' (ETyped exp typ) = 
          case exp of
                    EInt int -> emit $ "ldc " ++ show int
                    EDouble doub -> emit $ "ldc2_w " ++ show doub
                    ETrue -> emit "bipush 1 "
                    EFalse -> emit "bipush 0 "
                    EId id -> do
                            addr <- lookupAddr id
                            let load_string = if typ == TDouble
                                              then "dload "
                                              else "iload "
                            emit $ load_string ++ show addr 

                    ECall id@(Id "printDouble") argExps -> do
                      mapM_ (compileExp TDouble) argExps
                      sig <- getSig id
                      emit $ "invokestatic " ++ sig  
                    ECall id argExps -> do
                      mapM_ compileExp' argExps
                      sig <- getSig id
                      emit $ "invokestatic " ++ sig
                    EInc id -> do
                       addr <- lookupAddr id
                       case typ of
                        TDouble -> do  
                                  emit $ "dload " ++ show addr
                                  emit $ "dup2"
                                  emit $ "dconst_1"
                                  emit $ "dadd"
                                  emit $ "dstore " ++ show addr

                        _ ->do emit $ "iload " ++ show addr
                               emit $ "iinc " ++ show addr ++ " 1"
                    EDec id -> do
                      addr <- lookupAddr id
                      case typ of
                        TDouble ->do 
                                  emit $ "dload " ++ show addr
                                  emit $ "dup2"
                                  emit $ "dconst_1"
                                  emit $ "dsub"
                                  emit $ "dstore " ++ show addr
                        _ -> do 
                             emit $ "iload " ++ show addr
                             emit $ "iinc " ++ show addr ++ " -1"
                    EInc2 id ->do
                      addr <- lookupAddr id
                      
                      case typ of
                        TDouble ->do 
                          emit $ "dload "  ++ show addr
                          emit "dconst_1"
                          emit "dadd"
                          emit "dup2"
                          emit $ "dstore " ++ show addr
                        _ ->do 
                            emit $ "iinc "  ++ show addr ++ " 1"
                            emit $ "iload " ++ show addr
                    EDec2 id ->do
                      addr <- lookupAddr id
                      
                      case typ of
                        TDouble ->do
                          emit $ "dload "  ++ show addr
                          emit "dconst_1"
                          emit "dsub"
                          emit "dup2"
                          emit $ "dstore " ++ show addr
                        _ ->do
                          emit $ "iinc " ++ show addr ++ " -1" 
                          emit $ "iload " ++ show addr
                    EMul exp1 exp2 -> arethOp exp1 exp2 "mul"
                    EDiv  exp1 exp2 ->  arethOp exp1 exp2 "div"
                    EAdd  exp1 exp2 ->  arethOp exp1 exp2 "add"  
                    ESub  exp1 exp2 ->  arethOp exp1 exp2 "sub"
                    ELess exp1 exp2 -> do
                      let t = inferBin exp1 exp2
                      case t of
                        TDouble -> doubleCompare t exp1 exp2 "iflt "
                        _ -> integerCompare t exp1 exp2 "if_icmplt "
                    EGre exp1 exp2 -> do
                      let t = inferBin exp1 exp2
                      case t of 
                        TDouble -> doubleCompare t exp1 exp2 "ifgt "
                        _ -> integerCompare t exp1 exp2 "if_icmpgt "
                    ELeq exp1 exp2 -> do
                      let t = inferBin exp1 exp2
                      case t of 
                        TDouble -> doubleCompare t exp1 exp2 "ifle "
                        _ ->  integerCompare t exp1 exp2 "if_icmple "
                    EGeq exp1 exp2 -> do
                      let t = inferBin exp1 exp2
                      case t of 
                        TDouble -> doubleCompare t exp1 exp2 "ifge "
                        _ -> integerCompare t exp1 exp2 "if_icmpge "
                    EEqua exp1 exp2 -> do
                      let t = inferBin' exp1 exp2
                      case t of
                        TDouble -> doubleCompare t exp1 exp2 "ifeq "
                        _ -> integerCompare t exp1 exp2 "if_icmpeq "
                    EIneq exp1 exp2 -> do
                      let t = inferBin' exp1 exp2
                      case t of
                        TDouble -> doubleCompare t exp1 exp2 "ifne "
                        _ -> integerCompare t exp1 exp2 "if_icmpne "
                    EConj exp1 exp2 -> do
                      compileExp' exp1
                      end <- newLabel "END"
                      emit "dup "
                      emit $ "ifeq " ++ end
                      emit "pop "
                      compileExp' exp2
                      emit $ end ++ ":"
                    EDisj exp1 exp2 -> do
                      compileExp' exp1
                      end <- newLabel "END"
                      emit "dup "
                      emit $ "ifne " ++ end
                      emit "pop "
                      compileExp' exp2
                      emit $ end ++ ":"
                    EAss id exp -> do
                      compileExp typ exp
                      addr <- lookupAddr id
                      case typ of
                        TDouble ->do
                           emit "dup2"
                           emit $ "dstore " ++ show addr
                        _ -> do
                          emit "dup"
                          emit $ "istore " ++ show addr
            where 
                          integerCompare :: Type -> Exp -> Exp -> String -> State Env ()
                          integerCompare t exp1 exp2 op =
                            do
                              compileExp t exp1
                              compileExp t exp2
                              true <- newLabel "TRUE"
                              end <- newLabel "END"
                              emit $ op ++ true
                              emit "bipush 0 "
                              emit $ "goto " ++ end
                              emit $ true ++ ":"
                              emit "bipush 1"
                              emit $ end ++ ":"
                          doubleCompare :: Type -> Exp -> Exp -> String -> State Env ()
                          doubleCompare t exp1 exp2 op = do 
                            compileExp t exp1
                            compileExp t exp2
                            true <- newLabel "TRUE"
                            end <- newLabel "END"
                            emit "dcmpl "
                            emit $ op ++ true
                            emit "bipush 0"
                            emit $ "goto " ++ end
                            emit $ true ++ ":"
                            emit "bipush 1"
                            emit $ end ++ ":"
                          arethOp :: Exp -> Exp -> String -> State Env ()
                          arethOp exp1 exp2 op = do
                            let typ = inferBin exp1 exp2 
                            compileExp typ exp1
                            compileExp typ exp2
                            case typ of 
                              TDouble -> emit $ "d" ++ op
                              TInt -> emit $ "i" ++ op
                            emit $ ";; " ++ show exp1
                            emit $ ";; " ++ show exp2

                            

                            

                      
inferBin :: Exp -> Exp -> Type
inferBin (ETyped _ typ1) (ETyped _ typ2) = 
                    case typ1 of
                      TDouble -> TDouble
                      TInt -> case typ2 of
                        TDouble -> TDouble
                        TInt -> TInt   

inferBin' :: Exp -> Exp -> Type
inferBin' (ETyped _ TBool) (ETyped _ TBool) = TBool
inferBin' exp1 exp2 = inferBin exp1 exp2


compileFun :: Func -> State Env ()
compileFun (DFun typ id args body) = do
            funString <- lookupFun id
            emit $ ".method public static " ++ funString
            emit ".limit locals 100"
            emit ".limit stack 1000"
            mapM_  (\(FArgs typ id) -> extendId typ id) args 
            mapM_ compileStm body
            when (typ == TVoid) $ emit "return"
            emit "ireturn"
            emit' ".end method"
            emit ""

emit :: Instruction -> State Env ()
emit i = emit' $ " " ++ i

emit' :: String -> State Env ()
emit' str = modify (\env -> env{code = str : code env})


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
            where lookupAddr' (ctx:stack) = fromMaybe (lookupAddr' stack) (Map.lookup id ctx)
                  

lookupFun :: Id -> State Env FunString
lookupFun id = case id of
              (Id "printInt") -> return "printInt(I)V"
              (Id "printDouble") -> return "printDouble(D)V"
              (Id "readInt") -> return "readInt()I"
              (Id "readDouble") -> return "readDouble()D"
              id -> gets $ fromJust . Map.lookup id . funs

              
extendId :: Type -> Id -> State Env Int
extendId typ id = do
    env <- get
    let (ctx:ctxs) = vars env
    let varPos = maxvar env
    modify (\env -> env{vars = (Map.insert id varPos ctx):ctxs, maxvar = if typ == TDouble then  varPos + 2 else  varPos + 1})
    return varPos

extendFunc :: Env -> Func -> Env
extendFunc env func@(DFun _ id _ _) = env{funs = Map.insert id (rewriteFunc func) (funs env)} 

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

