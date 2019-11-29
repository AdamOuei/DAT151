module Interpreter where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM


type Env = (Sig,[Context]) -- functions and context stack
type Sig = Map Id ([Type],Type) -- function type signature
type Context = Map Id (Maybe Val) -- variables with their types

data Val = VInt Integer | VDouble Double | VBool Bool | VVoid
    deriving (Eq,Show)

interpret :: Program -> IO ()
interpret p = putStrLn "no interpreter yet"

evalExp ::  Env -> Exp -> IO (Val, Env)
evalExp env@(sig,top:context) exp =  case exp of
                        EInt int -> return (VInt int, env)
                        EDouble doub -> return (VDouble doub, env)
                        ETrue -> return (VBool True, env)
                        EFalse -> return (VBool True, env)
                        EId id -> do 
                            val <- lookupVar env id 
                            return (val,env)
                        ECall id argExps ->
                                        case lookupFun env id of
                                            Bad s -> Bad s
                                            Ok (argsTypes, typ) -> do
                                                                    mapM_ (uncurry $ checkExp env) $ zip  argsTypes argExps
                                                                    return typ
                        -- Maybe change to TBool and add void
                        EInc id -> case lookupVar env id of
                                    Ok TBool -> Bad "Not valid type"
                                    _ -> lookupVar env id
                        EDec id ->  case lookupVar env id of
                                    Ok TBool -> Bad "Not valid type"
                                    _ -> lookupVar env id
                        EInc2 id ->  case lookupVar env id of
                                    Ok TBool -> Bad "Not valid type"
                                    _ -> lookupVar env id
                        EDec2 id ->  case lookupVar env id of
                                    Ok TBool -> Bad "Not valid type"
                                    _ -> lookupVar env id
                        EMul exp1 exp2 -> inferBin env exp1 exp2
                        EDiv exp1 exp2 -> inferBin env exp1 exp2
                        EAdd exp1 exp2 -> inferBin env exp1 exp2
                        ESub exp1 exp2 -> inferBin env exp1 exp2
                        ELess exp1 exp2 -> case inferBin env exp1 exp2 of
                                            Ok _ -> Ok TBool
                                            _ -> Bad "Bad type"                
                        EGre exp1 exp2 -> case inferBin env exp1 exp2 of
                                            Ok _ -> Ok TBool
                                            _ -> Bad "Bad type"
                        ELeq exp1 exp2 -> case inferBin env exp1 exp2 of
                                            Ok _ -> Ok TBool
                                            _ -> Bad "Bad type"
                        EGeq exp1 exp2 -> case inferBin env exp1 exp2 of
                                            Ok _ -> Ok TBool
                                            _ -> Bad "Bad type"
                        EEqua exp1 exp2 -> case inferExp env exp1 of
                                            Ok TBool -> case inferExp env exp2 of
                                                        Ok TBool -> Ok TBool
                                                        _ -> Bad "Bad type"
                                            Bad _ -> Bad "Bad type"
                                            _ -> case inferExp env exp2 of
                                                        Ok TBool -> Bad "Bad type"
                                                        Bad _ -> Bad "Bad type"
                                                        _ -> Ok TBool
                        EIneq exp1 exp2 -> case inferExp env exp1 of
                                            Ok TBool -> case inferExp env exp2 of
                                                        Ok TBool -> Ok TBool
                                                        _ -> Bad "Bad type"
                                            Bad _ -> Bad "Bad type"
                                            _ -> case inferExp env exp2 of
                                                        Ok TBool -> Bad "Bad type"
                                                        Bad _ -> Bad "Bad type"
                                                        _ -> Ok TBool
                        EConj exp1 exp2 -> case inferExp env exp1 of
                                            Ok TBool -> case inferExp env exp2 of
                                                        Ok TBool -> Ok TBool
                                                        _ -> Bad "Wrong type"
                                            Bad _ -> Bad "Not valid type"
                        EDisj exp1 exp2 -> case inferExp env exp1 of
                                            Ok TBool -> case inferExp env exp2 of
                                                        Ok TBool -> Ok TBool
                                                        _ -> Bad "Wrong type"
                                            Bad _ -> Bad "Not valid type"
                        EAss id exp -> case lookupVar env id of
                                        Bad s -> Bad s
                                        Ok typ -> case checkExp env typ exp of
                                                    Bad s -> Bad s
                                                    Ok _ -> Ok typ 
                        ETyped exp typ -> case checkExp env typ exp of
                                            Ok _ -> Ok typ
                                            Bad s -> Bad s

execProg :: Env -> IO ()

execStm :: Env -> Stm -> IO (Maybe Val, Env)
execStm env@(sig,top:context) stm = case stm of 
        SExp exp -> do
        SDecls typ (id:ids) -> 
        SDecls typ [] -> 
        SInit typ id exp -> 
        SRet exp -> 
        SWhile exp stm -> 
        SIf exp stm1 stm2 -> 
        SBlock stmxs -> 
                

lookupVar :: Env -> Id -> IO Val
lookupVar (_, []) _ = fail "No variable found"
lookupVar (sig,first:context) id = case Map.lookup id first of
                                    Just (Just val) -> return val
                                    Just Nothing -> fail "Variable not found"
                                    Nothing -> fail "Variable not found"

lookupFun :: Env -> Id -> IO Func
lookupFun (sig, _) id = case Map.lookup id sig of
                            Nothing -> fail "Function not found"
                            Just fun -> return fun

exitBlock :: Env -> Env
exitBlock (sig, firstBlock:context)= (sig, context) 

newBlock :: Env -> Env
newBlock (sig,context) = (sig, Map.empty:context)

emptyEnv :: Env
emptyEnv  = (Map.empty,[Map.empty])

addValue :: Val -> Val -> Val
addValue (VInt a) (VInt b) = VInt $ a + b 
addValue (VDouble a) (VDouble b) = VDouble $ a + b 
addValue (VDouble a) (VInt b) = VDouble $ a + fromIntegral b

subValue :: Val -> Val -> Val
subValue (VInt a) (VInt b) = VInt $ a-b
subValue (VDouble a) (VDouble b) = VDouble $ a-b
subValue (VDouble a) (VInt b) = VDouble $ a - fromIntegral b

mulValue :: Val -> Val -> Val
mulValue (VInt a) (VInt b) = VInt $ a*b
mulValue (VDouble a) (VDouble b) = VDouble $ a*b

divValue :: Val -> Val -> Val
divValue (VInt a) (VInt b) = VInt $ a `div` b
divValue (VDouble a) (VDouble b) = VDouble $ a/b

greaterThanValue :: Val -> Val -> Val
greaterThanValue (VInt a) (VInt b) = VBool $ a > b
greaterThanValue (VDouble a) (VDouble b) = VBool $ a > b

lessThanValue :: Val -> Val -> Val
lessThanValue (VInt a) (VInt b) = VBool $ a<b
lessThanValue (VDouble a) (VDouble b) = VBool $ a<b

greaterThanEqualValue :: Val -> Val -> Val
greaterThanEqualValue (VInt a) (VInt b) = VBool $ a >= b
greaterThanEqualValue (VDouble a) (VDouble b) = VBool $ a >= b

lessThanEqualValue :: Val -> Val -> Val
lessThanEqualValue (VInt a) (VInt b) = VBool $ a<=b
lessThanEqualValue (VDouble a) (VDouble b) = VBool $ a<=b

equalValue :: Val -> Val -> Val
equalValue (VInt a) (VInt b) = VBool $ a == b
equalValue (VDouble a) (VDouble b) = VBool $ a == b

notEqualValue :: Val -> Val -> Val
notEqualValue (VInt a) (VInt b) = VBool $ a/=b
notEqualValue (VDouble a) (VDouble b) = VBool $ a/=b







