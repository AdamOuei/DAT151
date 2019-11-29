module Interpreter where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import CMM.Abs
import CMM.Print
import CMM.ErrM


type Env = (Sig,[Context]) -- functions and context stack
type Sig = Map Id Func-- function type signature
type Context = Map Id (Maybe Val) -- variables with their types

data Val = VInt Integer | VDouble Double | VBool Bool | VVoid
    deriving (Eq,Show)

interpret :: Program -> IO ()
interpret (Prg funcs) = do
    env <- foldM updateFun emptyEnv funcs
    execProg env 

evalExp ::  Env -> Exp -> IO (Val, Env)
evalExp env@(sig,top:context) exp =  case exp of
                        EInt int -> return (VInt int, env)
                        EDouble doub -> return (VDouble doub, env)
                        ETrue -> return (VBool True, env)
                        EFalse -> return (VBool True, env)
                        EId id -> do 
                            val <- lookupVar env id 
                            return (val,env)
                        -- This must be done!
                        ECall id argExps -> return (VBool True,env)
                                        
                        -- Maybe change to TBool and add void
                        EInc id -> do
                                    val <- lookupVar env id
                                    let newVal = addValue val (VInt 1)
                                    newEnv <- updateVar env id newVal
                                    return (newVal, newEnv)
                        EDec id ->  do
                                    val <- lookupVar env id
                                    let newVal = subValue val (VInt 1)
                                    newEnv <- updateVar env id newVal
                                    return (newVal, newEnv)
                        EInc2 id ->  do
                                        val <- lookupVar env id
                                        let newVal = addValue val (VInt 1)
                                        newEnv <- updateVar env id newVal
                                        return (val,newEnv)
                        EDec2 id ->  do
                                        val <- lookupVar env id
                                        let newVal = subValue val (VInt 1)
                                        newEnv <- updateVar env id newVal
                                        return (val,newEnv)
                        EMul exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = mulValue val1 val2
                                            return (newVal, env2)
                        EDiv exp1 exp2 ->do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = divValue val1 val2
                                            return (newVal, env2)
                        EAdd exp1 exp2 ->do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = subValue val1 val2
                                            return (newVal, env2)
                        ESub exp1 exp2 ->do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = subValue val1 val2
                                            return (newVal, env2)
                        ELess exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = lessThanValue val1 val2
                                            return (newVal, env2)
                        EGre exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = greaterThanValue val1 val2
                                            return (newVal, env2)
                        ELeq exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = lessThanEqualValue val1 val2
                                            return (newVal, env2)
                        EGeq exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = greaterThanEqualValue val1 val2
                                            return (newVal, env2)
                        EEqua exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = equalValue val1 val2
                                            return (newVal, env2)
                        EIneq exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env exp2
                                            let newVal = notEqualValue val1 val2
                                            return (newVal, env2)
                        EConj exp1 exp2 -> do
                                            (val1, env1) <- evalExp env exp1
                                            case val1 of
                                                VBool True -> return (VBool True, env1)
                                                VBool False -> evalExp env exp2
                        EDisj exp1 exp2 -> do
                                            (val1,env1) <- evalExp env exp1
                                            case val1 of 
                                                VBool True -> evalExp env1 exp2
                                                VBool False -> return (VBool False,env1)
                        EAss id exp -> do
                                        (val1,env1) <- evalExp env exp 
                                        env2 <- updateVar env1 id val1
                                        return (val1,env2)

execProg :: Env -> IO ()
execProg env = do
    FDef _ _ _ (FBody stms) <- lookupFun env (Id "main")
    execStms env stms
    return ()


execStms :: Env -> [Stm] -> IO (Maybe Val, Env)
execStms env [] = return (Nothing, env)
execStms env (stm:stms) = do
                    (newVal,newEnv) <- execStm env stm
                    if isJust newVal
                        then return (newVal,newEnv)
                        else execStms newEnv stms


execStm :: Env -> Stm -> IO (Maybe Val, Env)
execStm env@(sig,top:context) stm = case stm of 
        SExp exp -> do
                (val, env2) <- evalExp env exp
                return (Nothing, env2)
        SDecls typ ids -> return (Nothing, foldl declareVar env ids)
        
        SInit typ id exp -> do
            (val1, env1) <- evalExp env exp
            return (Nothing, initVar env1 id val1)
        SRet exp -> do
            (val1,env1) <- evalExp env exp
            return (Just val1, env1)
        SWhile exp stm1 -> do
                (val1, env1) <- evalExp env exp
                if val1 == VBool True
                    then do 
                        (val2, env2) <- execStm (newBlock env1) stm1
                        let newEnv = exitBlock env2
                        if isJust val2
                            then return (val2,newEnv)
                            else execStm newEnv stm
                    else 
                        return (Nothing,env1)     
        SIf exp stm1 stm2 -> do
                    (val1,env1) <- evalExp env exp
                    let newStm =  if val1 == VBool True 
                        then stm1
                        else stm2
                    (newVal,env2) <- execStm (newBlock env1) newStm
                    return (newVal, exitBlock env2)
        SBlock [] -> return (Nothing, env)
        SBlock stms -> do
                (newVal, newEnv) <- execStms (newBlock env) stms
                return (newVal, exitBlock newEnv)

                        


declareVar :: Env -> Id -> Env
declareVar (sig,top:context) id = (sig, newTop:context)
                where newTop = Map.insert id Nothing top

initVar :: Env -> Id -> Val -> Env
initVar (sig, top:context) id val = (sig, newTop:context) 
                where newTop = Map.insert id (Just val) top

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

-- Fix this to work withn interpreters
updateVar :: Env -> Id -> Val -> IO Env
updateVar (sig, top:stack) id val = 
                    if Map.member id top 
                    then return (sig, newTop:stack)
                    else do
                        (newSig, newContext) <- updateVar (sig, stack) id val
                        return (newSig,top:newContext)
                    
                    where newTop = Map.insert id (Just val) top
updateVar _ id _ = fail "Can't find variable for id"                        
                
updateFun :: Env -> Func -> IO Env
updateFun (sig,context) func@(FDef _ id _ _ ) =
    if Map.member id sig
    then fail "Function alreadty exists"
    else return (Map.insert id func sig , context)


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







