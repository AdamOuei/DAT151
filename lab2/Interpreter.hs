module Interpreter where

import Control.Monad
import Control.Exception

import System.Exit 

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

data RunTimeException = VariableNotFoundException
    deriving Show

instance Exception RunTimeException

interpret :: Program -> IO ()
interpret (Prg funcs) = do
    env <- foldM updateFun emptyEnv funcs
    execProg env 

evalExps :: Env -> [Exp] -> IO ([Val],Env)
evalExps env [] = return ([],env)     
evalExps env (exp:exps) = do
    (val1,newEnv) <- evalExp env exp
    (val2,newerEnv) <- evalExps newEnv exps
    return (val1:val2, newerEnv)

evalExp ::  Env -> Exp -> IO (Val, Env)
evalExp env exp =  case exp of
                        EInt int -> return (VInt int, env)
                        EDouble doub -> return (VDouble doub, env)
                        ETrue -> return (VBool True, env)
                        EFalse -> return (VBool False, env)
                        EId id -> do 
                            val <- lookupVar env id 
                            return (val,env)
                        ECall id@(Id i) argExps -> do
                            (vals,newEnv@(sig,context)) <- evalExps env argExps
                            if i `elem` ["printInt", "printDouble", "readInt", "readDouble"]
                                then do
                                    val <- checkBuiltIn id vals
                                    return (val, newEnv)
                                else case argExps of 
                                        [] -> do
                                            func <- lookupFun env id
                                            execFunc env func []
                                        _ -> do
                                            func <- lookupFun env id 
                                            --(vals',(sig,context)) <- evalExps env argExps
                                            (val',newEnv') <- execFunc (sig,[]) func vals
                                            return (val',(sig,context))  
                        EInc2 id -> do
                                    val <- lookupVar env id
                                    let newVal = addValue val (VInt 1)
                                    newEnv <- updateVar env id newVal
                                    return (newVal, newEnv)
                        EDec2 id ->  do
                                    val <- lookupVar env id
                                    let newVal = subValue val (VInt 1)
                                    newEnv <- updateVar env id newVal
                                    return (newVal, newEnv)
                        EInc id ->  do
                                        val <- lookupVar env id
                                        let newVal = addValue val (VInt 1)
                                        newEnv <- updateVar env id newVal
                                        return (val,newEnv)
                        EDec id ->  do
                                        val <- lookupVar env id
                                        let newVal = subValue val (VInt 1)
                                        newEnv <- updateVar env id newVal
                                        return (val,newEnv)
                        EMul exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = mulValue val1 val2
                                            return (newVal, env2)
                        EDiv exp1 exp2 ->do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = divValue val1 val2
                                            return (newVal, env2)
                        EAdd exp1 exp2 ->do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = addValue val1 val2
                                            return (newVal, env2)
                        ESub exp1 exp2 ->do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = subValue val1 val2
                                            return (newVal, env2)
                        ELess exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = lessThanValue val1 val2
                                            return (newVal, env2)
                        EGre exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = greaterThanValue val1 val2
                                            return (newVal, env2)
                        ELeq exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = lessThanEqualValue val1 val2
                                            return (newVal, env2)
                        EGeq exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = greaterThanEqualValue val1 val2
                                            return (newVal, env2)
                        EEqua exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = equalValue val1 val2
                                            return (newVal, env2)
                        EIneq exp1 exp2 -> do 
                                            (val1,env1) <- evalExp env exp1
                                            (val2,env2) <- evalExp env1 exp2
                                            let newVal = notEqualValue val1 val2
                                            return (newVal, env2)
                        EConj exp1 exp2 -> do
                                            (val1, env1) <- evalExp env exp1
                                            case val1 of
                                                VBool True -> evalExp env exp2
                                                VBool False ->return (VBool False, env1) 
                        EDisj exp1 exp2 -> do
                                            (val1,env1) <- evalExp env exp1
                                            case val1 of 
                                                VBool True -> return (VBool True,env1)
                                                VBool False -> evalExp env1 exp2
                        EAss id exp -> do
                                        (val1,env1) <- evalExp env exp 
                                        env2 <- updateVar env1 id val1
                                        return (val1,env2)


--evalCall :: Env -> Id -> Arguments -> IO (Val, Env)
--evalCall env id args = do
  --                       (FDef typ id args body) <- lookupFun env id


execProg :: Env -> IO ()
execProg env = do
    FDef _ _ _ (FBody stms) <- lookupFun env (Id "main")
    execStms env stms
    return ()

checkBuiltIn :: Id -> [Val] -> IO Val
checkBuiltIn (Id "printInt") (VInt i:_) = do
    print i
    return VVoid
checkBuiltIn (Id "printDouble") (val:_) = do
    print $ toDouble val
    return VVoid
checkBuiltIn (Id "readInt") _ = VInt <$> readLn
checkBuiltIn (Id "readDouble") _ = VDouble <$> readLn

toDouble :: Val -> Double
toDouble val = case val of
    VDouble d -> d
    VInt i -> fromIntegral i

execStms :: Env -> [Stm] -> IO (Maybe Val, Env)
execStms env [] = return (Nothing, env)
execStms env (stm:stms) = do
                    (newVal,newEnv) <- execStm env stm
                    --execStms newEnv stms
                    case newVal of
                        Just (Ret val) -> return (newVal, newEnv)
                        _ -> execStms newEnv stms
                    

execFunc :: Env -> Func -> [Val] -> IO(Val,Env)
execFunc env (FDef typ id (FArgument args) (FBody body)) vals = do
    let params = zip args vals
    let newEnv = foldl foldlHelp (newBlock env) params
    (retVal, newEnv') <- execStms newEnv body
    case retVal of
        Just (Ret val) -> return (val, exitBlock newEnv')
        _ -> return (VVoid, exitBlock newEnv')

foldlHelp :: Env -> (Args, Val) -> Env
foldlHelp env (FArgs _ id, val) = initVar env id val


execStm :: Env -> Stm -> IO (Maybe Val, Env)
execStm env stm = case stm of  
        SExp exp -> do
                (val, env2) <- evalExp env exp
                return (Just val, env2)
        SDecls typ ids -> return (Nothing, foldl declareVar env ids) 
        SInit typ id exp -> do
            (val1, env1) <- evalExp env exp
            return (Just val1, initVar env1 id val1)
        SRet exp -> do
            (val1,env1) <- evalExp env exp
            return (Just (Ret val1), env1)
        SWhile exp stm1 -> do
                (val1, env1) <- evalExp env exp
                if val1 == VBool True
                    then do 
                        (val2, env2) <- execStm (newBlock env1) stm1
                        let newEnv = exitBlock env2
                        if isRet val2
                            then return (val2,newEnv)
                            else do
                            (val3, env3) <- execStm (newBlock newEnv) (SWhile exp stm1)
                            return (val3, env3)
                        --if isJust val2
                        --   then return (val2 ,newEnv)
                        --   else execStm newEnv stm
                else 
                    return (Nothing, env1)     
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

                

isRet :: Maybe Val -> Bool
isRet val = case val of 
        Just (Ret _ ) -> True
        _ -> False



declareVar :: Env -> Id -> Env
declareVar (sig,top:context) id = (sig, newTop:context)
                where newTop = Map.insert id Nothing top

initVar :: Env -> Id -> Val -> Env
initVar (sig, []) _ _ = (sig, [])
initVar (sig, top:context) id val = (sig, newTop:context) 
                where newTop = Map.insert id (Just val) top

lookupVar :: Env -> Id -> IO Val
lookupVar (_, []) _ =do
         putStrLn "INTERPRETER ERROR"
         exitFailure
lookupVar (sig,first:context) id = case Map.lookup id first of
                                    Just (Just val) -> return val
                                    Just Nothing -> do
                                        putStrLn "INTERPRETER ERROR"
                                        exitFailure
                                    Nothing -> lookupVar (sig, context) id

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
updateVar _ id _ = fail $ "Can't find variable for id" ++ printTree id               
                
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
addValue  (VInt a )(VDouble b)  = VDouble $ fromIntegral a +  b

subValue :: Val -> Val -> Val
subValue (VInt a) (VInt b) = VInt $ a-b
subValue (VDouble a) (VDouble b) = VDouble $ a-b
subValue (VDouble a) (VInt b) = VDouble $ a - fromIntegral b
subValue  (VInt a )(VDouble b)  = VDouble $ fromIntegral a - b

mulValue :: Val -> Val -> Val
mulValue (VInt a) (VInt b) = VInt $ a*b
mulValue (VDouble a) (VDouble b) = VDouble $ a*b
mulValue (VDouble a) (VInt b) = VDouble $ a * fromIntegral b
mulValue (VInt a) (VDouble b) = VDouble $ fromIntegral a * b

divValue :: Val -> Val -> Val
divValue (VInt a) (VInt b) = VInt $ a `div` b
divValue (VDouble a) (VDouble b) = VDouble $ a/b
divValue (VDouble a) (VInt b) = VDouble $ a / fromIntegral b
divValue (VInt a) (VDouble b) = VDouble $ fromIntegral a / b

greaterThanValue :: Val -> Val -> Val
greaterThanValue (VInt a) (VInt b) = VBool $ a > b
greaterThanValue (VDouble a) (VDouble b) = VBool $ a > b
greaterThanValue val1 val2 = case val1 of 
                            VDouble a -> case val2 of
                                VInt b -> VBool $ a > fromIntegral b
                            VInt a -> case val2 of 
                                VDouble b -> VBool $ fromIntegral a < b

lessThanValue :: Val -> Val -> Val
lessThanValue (VInt a) (VInt b) = VBool $ a<b
lessThanValue (VDouble a) (VDouble b) = VBool $ a<b
lessThanValue val1 val2 = case val1 of 
    VDouble a -> case val2 of
        VInt b -> VBool $ a > fromIntegral b
    VInt a -> case val2 of 
        VDouble b -> VBool $ fromIntegral a < b

greaterThanEqualValue :: Val -> Val -> Val
greaterThanEqualValue (VInt a) (VInt b) = VBool $ a >= b
greaterThanEqualValue (VDouble a) (VDouble b) = VBool $ a >= b
greaterThanEqualValue (VInt a) (VDouble b) = VBool $ fromIntegral a >= b
greaterThanEqualValue (VDouble a) (VInt b) = VBool $ a >= fromIntegral b

lessThanEqualValue :: Val -> Val -> Val
lessThanEqualValue (VInt a) (VInt b) = VBool $ a<=b
lessThanEqualValue (VDouble a) (VDouble b) = VBool $ a<=b
lessThanEqualValue (VInt a) (VDouble b) = VBool $ fromIntegral a<=b
lessThanEqualValue (VDouble a) (VInt b) = VBool $ a<= fromIntegral b

equalValue :: Val -> Val -> Val
equalValue (VInt a) (VInt b) = VBool $ a == b
equalValue (VDouble a) (VDouble b) = VBool $ a == b
equalValue (VInt a) (VDouble b) = VBool $ fromIntegral a == b
equalValue (VDouble a) (VInt b) = VBool $ a == fromIntegral b
equalValue (VBool a) (VBool b) = VBool $ a == b


notEqualValue :: Val -> Val -> Val
notEqualValue (VInt a) (VInt b) = VBool $ a/=b
notEqualValue (VDouble a) (VDouble b) = VBool $ a/=b
notEqualValue (VInt a) (VDouble b) = VBool $ fromIntegral a /= b
notEqualValue (VDouble a) (VInt b) = VBool $ a /= fromIntegral b
notEqualValue (VBool a) (VBool b) = VBool $ a /= b






