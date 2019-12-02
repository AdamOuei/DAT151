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

data Val = VInt Integer | VDouble Double | VBool Bool | VVoid | Ret Val
    deriving (Eq,Show)

interpret :: Program -> IO ()
interpret (Prg funcs) = do
    (sig, ctxs) <- foldM updateFun emptyEnv funcs
    case Map.lookup (Id "main") sig of
        Nothing -> "main missing"
        Just func -> do
                        execFunc env func []
                        return ()

evalExp ::  Env -> Exp -> IO (Val, Env)
evalExp env@(sig,top:context) exp =  case exp of
                        EInt int -> return (VInt int, env)
                        EDouble doub -> return (VDouble doub, env)
                        ETrue -> return (VBool True, env)
                        EFalse -> return (VBool True, env)
                        EId id -> do 
                            val <- lookupVar env id 
                            return (val,env)
                        -- This must be done! TODO!!!
                        ECall id argExps -> evalCall env id argExps 
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
                                            let newVal = addValue val1 val2
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
                                                VBool True -> evalExp env1 exp2
                                                VBool False -> return (val1, env1) 
                        EDisj exp1 exp2 -> do
                                            (val1,env1) <- evalExp env exp1
                                            case val1 of 
                                                VBool True -> return (val1, env1) 
                                                VBool False -> evalExp env1 exp2
                        EAss id exp -> do
                                        (val, env1) <- evalExp env exp 
                                        env2 <- updateVar env1 id val
                                        return (val, env2)


evalCall :: Env -> Id -> [Exp] -> IO (Val, Env)
evalCall env id argExps = case id of
    Id "printInt" -> do
        (VInt int, newEnv) <- map (evalExp env) argExps
        print int
        return (VVoid, newEnv)
    Id "printDouble" -> do
        (VDouble double, newEnv) <- map (evalExp env) argExps
        print double
        return (VVoid, newEnv)
    Id "readInt" -> do
        input <- getLine
        let parsedInput = read input :: Integer
        return (VInt parsedInput, env)
    Id "readDouble" -> do
        input <- getLine
        let parsedInput = read input :: Double
        return (VDouble parsedInput, env)
    _ -> case argExps of
        [] -> do
            let func = lookupFun env id
            execFunc env id []
        _ -> do
            let func = lookupFun env id
            (vals, (sig, ctx)) <- evalArgs en exps
            (val, newEnv) <- execFunc (sig, []) func vals
            return (val, (sig, ctx))

execFunc :: Env -> Func -> [Val] -> IO (Val, Env)
execFunc env (FDef typ id args body) vals = do
    let params = zip args vals
    let newEnv = map (\((Arg _ id), val) -> updateVar env id val) params
    (retVal, newEnv') <- execStms newEnv body
    case retVal of
        Ret val -> return (val, exitBlock newEnv')
        _ -> return (VVoid, exitBlock newEnv')

execStms :: Env -> [Stm] -> IO (Val, Env)
execStms env [] = return (VVoid, env)
execStms env (stm:stms) = do
                    (newVal,newEnv) <- execStm env stm
                    case newVal of
                        Return _ -> return (val, newEnv)
                        _ -> execStms newEnv stms

execStm :: Env -> Stm -> IO (Val, Env)
execStm env@(sig,top:context) stm = case stm of 
        SExp exp -> evalExp env exp
        SDecls typ ids -> case typ of
            TVoid -> return (VVoid, env)
            _ -> return (VVoid, map (\id -> initVar env id VVoid ))        
        SInit typ id exp -> do
            (val1, env1) <- evalExp env exp
            return (val1, initVar env1 id val1)
        SRet exp -> do
            (val1,env1) <- evalExp env exp
            return (Return val1, env1)
        SWhile exp stm1 -> do
                (val1, env1) <- evalExp env exp
                if val1 == VBool True
                    then do 
                        (val2, env2) <- execStm (newBlock env1) stm1
                        let newEnv = exitBlock env2
                        case val2 of
                            Return _ -> return (VVoid, exitBlock env1)
                            _ -> execStm (exitBlock env2) (SWhile exp stm1)
                    else 
                        return (VVoid, exitBlock env1)     
        SIf exp stm1 stm2 -> do
                    (val1,env1) <- evalExp env exp
                    case val1 of
                        (VBool True) -> do
                            (val2, env2) <- execStm env1 stm1
                            return (val2, exitBlock env2)
                        (VBool False) -> do
                            (val2, env2) <- execStm env1 stm2
                            return (val2, exitBlock env2)
        SBlock stms -> do
                (newVal, newEnv) <- execStms (newBlock env) stms
                return (newVal, exitBlock newEnv)

                        


declareVar :: Env -> Id -> Env
declareVar (sig,top:context) id = (sig, newTop:context)
                where newTop = Map.insert id Nothing top

initVar :: Env -> Id -> Val -> Env
initVar (sig, top:context) id val = (sig, newTop:context) 
                where newTop = Map.insert id val top

lookupVar :: Env -> Id -> IO Val
lookupVar (_, []) _ = fail "No variable found"
lookupVar (sig,first:context) id = case Map.lookup id first of
                                    Just VVoid -> fail "Void has no value"
                                    Just val -> return val
                                    Nothing -> lookupVar (sig, context) id

lookupFun :: Env -> Id -> Func
lookupFun (sig, _) id = case Map.lookup id sig of
                            Just fun -> fun

-- Fix this to work withn interpreters
updateVar :: Env -> Id -> Val -> Env
updateVar (sig, ctx@(top:stack)) id val = 
    case Map.lookup id top of
        Nothing -> do
            let (newSig, newStack) = updateVar (sig, stack) id val
            (newSig, top:newStack)
        Just _ -> (sig, Map.insert id val ctx)                       
                
updateFun :: Env -> Func -> Env
updateFun (sig,context) func = (Map.insert id func sig, context)

exitBlock :: Env -> Env
exitBlock (sig, top:context) = (sig, context) 

newBlock :: Env -> Env
newBlock (sig,context) = (sig, Map.empty:context)

emptyEnv :: Env
emptyEnv  = (Map.empty,[])

addValue :: Num a => a -> a -> a
addValue (VInt a) (VInt b) = VInt $ a + b 
addValue (VDouble a) (VDouble b) = VDouble $ a + b 
addValue (VDouble a) (VInt b) = VDouble $ a + fromIntegral b

subValue :: Num a => a -> a -> a
subValue (VInt a) (VInt b) = VInt $ a-b
subValue (VDouble a) (VDouble b) = VDouble $ a-b
subValue (VDouble a) (VInt b) = VDouble $ a - fromIntegral b

mulValue :: Num a => a -> a -> a
mulValue (VInt a) (VInt b) = VInt $ a*b
mulValue (VDouble a) (VDouble b) = VDouble $ a*b

divValue :: Num a => a -> a -> a
divValue (VInt a) (VInt b) = VInt $ a `div` b
divValue (VDouble a) (VDouble b) = VDouble $ a/b

greaterThanValue :: (Eq a, Ord a) => a -> a -> a
greaterThanValue (VInt a) (VInt b) = VBool $ a > b
greaterThanValue (VDouble a) (VDouble b) = VBool $ a > b

lessThanValue :: (Eq a, Ord a) => a -> a -> a
lessThanValue (VInt a) (VInt b) = VBool $ a<b
lessThanValue (VDouble a) (VDouble b) = VBool $ a<b

greaterThanEqualValue :: (Eq a, Ord a) => a -> a -> a
greaterThanEqualValue (VInt a) (VInt b) = VBool $ a >= b
greaterThanEqualValue (VDouble a) (VDouble b) = VBool $ a >= b

lessThanEqualValue :: (Eq a, Ord a) => a -> a -> a
lessThanEqualValue (VInt a) (VInt b) = VBool $ a<=b
lessThanEqualValue (VDouble a) (VDouble b) = VBool $ a<=b

equalValue :: (Eq a, Ord a) => a -> a -> a
equalValue (VInt a) (VInt b) = VBool $ a == b
equalValue (VDouble a) (VDouble b) = VBool $ a == b

notEqualValue :: (Eq a, Ord a) => a -> a -> a
notEqualValue (VInt a) (VInt b) = VBool $ a/=b
notEqualValue (VDouble a) (VDouble b) = VBool $ a/=b







