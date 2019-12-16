module TypeChecker where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set


import CMM.Abs
import CMM.Print
import CMM.ErrM

import qualified Annotated as A
    
type Env = (Sig,[Context]) -- functions and context stack
type Sig = Map Id ([Type],Type) -- function type signature
type Context = Map Id Type -- variables with their types

data FunType = FunType Type [Type]

typecheck :: Program -> Err A.Program
typecheck (Prg prog) = do 
    let funcs = map getTypes prog
    env <- foldM (\env (id,typ) -> updateFun env id typ ) emptyEnv funcs
    isMain env
    list <- mapM (checkDef env) prog     --[(Env, A.Func)]
    let funList = map snd list
    return (A.PDefs funList)

builtIn :: [(Id, ([Type], Type))]
builtIn = [
  (Id "printInt", ([TInt], TVoid)),
  (Id "printDouble", ([TDouble], TVoid)),
  (Id "readInt", ([], TInt)),
  (Id "readDouble", ([], TDouble))
  ]

isMain :: Env -> Err ()
isMain env = 
        case lookupFun env (Id "main") of
            Ok ([], TInt) -> return ()
            _ -> Bad "No function main"

            


inferExp :: Env -> Exp -> Err (Type,A.Exp)  
inferExp env exp = case exp of
                    EInt int -> Ok (TInt, A.EInt int)
                    EDouble doub -> Ok (TDouble, A.EDouble doub)
                    ETrue -> Ok (TBool, A.ETrue)
                    EFalse -> Ok (TBool, A.EFalse)
                    EId id -> do 
                                typ <- lookupVar env id
                                Ok (typ, A.EId id)
                    ECall id argExps -> do 
                                        (argsTypes,typ) <- lookupFun env id
                                        list <- mapM (\exp -> inferExp env exp) argExps
                                        let expTypes =  map fst list
                                        let argsExps = map snd list
                                        if argsTypes ==  expTypes then --and ls then
                                            return (typ, A.ECall id argsExps)
                                        else
                                            Bad "Function has wrong argument types"
                    EInc id -> do
                                typ <- lookupVar env id
                                if typ `elem` [TInt,TDouble] then
                                    return (typ, A.EInc id)
                                    else
                                        Bad $ "Id " ++ printTree id ++ " has wrong type"
                    EDec id -> do
                            typ <- lookupVar env id
                            if typ `elem` [TInt,TDouble] then
                                return (typ, A.EDec id)
                            else
                                Bad $ "Id " ++ printTree id ++ " has wrong type"
                    EInc2 id ->  do
                            typ <- lookupVar env id
                            if typ `elem` [TInt,TDouble] then
                                return (typ, A.EInc2 id)
                            else
                                Bad $ "Id " ++ printTree id ++ " has wrong type"
                    EDec2 id ->  do
                            typ <- lookupVar env id
                            if typ `elem` [TInt,TDouble] then
                                return (typ, A.EDec2 id)
                            else
                                Bad $ "Id " ++ printTree id ++ " has wrong type"
                    EMul exp1 exp2 -> do
                                        typ <- inferBin env exp1 exp2
                                        (_, exp1') <- inferExp env exp1
                                        (_, exp2') <- inferExp env exp2
                                        Ok (typ, A.EMul exp1' exp2')
                    EDiv exp1 exp2 -> do
                        typ <- inferBin env exp1 exp2
                        (_, exp1') <- inferExp env exp1
                        (_, exp2') <- inferExp env exp2
                        Ok (typ, A.EDiv exp1' exp2')
                    EAdd exp1 exp2 -> do
                        typ <- inferBin env exp1 exp2
                        (_, exp1') <- inferExp env exp1
                        (_, exp2') <- inferExp env exp2
                        Ok (typ, A.EAdd exp1' exp2')
                    ESub exp1 exp2 -> do
                        typ <- inferBin env exp1 exp2
                        (_, exp1') <- inferExp env exp1
                        (_, exp2') <- inferExp env exp2
                        Ok (typ, A.ESub exp1' exp2')
                    ELess exp1 exp2 -> do
                        inferBin env exp1 exp2
                        (_, exp1') <- inferExp env exp1
                        (_, exp2') <- inferExp env exp2
                        Ok (TBool, A.ELess exp1' exp2')
                    EGre exp1 exp2 -> do
                        inferBin env exp1 exp2  
                        (_, exp1') <- inferExp env exp1
                        (_, exp2') <- inferExp env exp2         
                        return (TBool, A.EGre exp1' exp2')
                    ELeq exp1 exp2 ->do
                        inferBin env exp1 exp2 
                        (_, exp1') <- inferExp env exp1
                        (_, exp2') <- inferExp env exp2          
                        Ok (TBool, A.ELeq exp1' exp2')
                    EGeq exp1 exp2 ->do
                        inferBin env exp1 exp2  
                        (_, exp1') <- inferExp env exp1
                        (_, exp2') <- inferExp env exp2         
                        Ok (TBool, A.EGeq exp1' exp2') 
                    EEqua exp1 exp2 -> do
                        (typ1, exp1') <- inferExp env exp1     
                        (typ2, exp2') <- inferExp env exp2
                        if (isValidType typ1 typ2) then return (TBool, A.EEqua exp1' exp2')
                        else
                            Bad "Not equal expressions equal"
                    EIneq exp1 exp2 -> do
                        (typ1, exp1') <- inferExp env exp1     
                        (typ2, exp2') <- inferExp env exp2
                        if (isValidType typ1 typ2) then return (TBool, A.EIneq exp1' exp2')
                        else
                            Bad "Not equal expressions inequal"
                    EConj exp1 exp2 -> do
                        (typ1, exp1') <- inferExp env exp1     
                        (typ2, exp2') <- inferExp env exp2
                        if typ1 == typ2  && typ1 == TBool then return (TBool, A.EConj exp1' exp2')
                        else
                            Bad "Not equal expressions conj"
                    EDisj exp1 exp2 -> do
                        (typ1, exp1') <- inferExp env exp1     
                        (typ2, exp2') <- inferExp env exp2
                        if typ1 == typ2  && typ1 == TBool then return (TBool, A.EDisj exp1' exp2')
                        else
                            Bad "Not equal expressions disj"
                    EAss id exp -> do
                                varTyp <- lookupVar env id
                                (expTyp,exp') <- inferExp env exp
                                if isValidAss varTyp expTyp then return (varTyp, A.EAss id exp')
                                else Bad "Not correct assignemnt" 
                    --ETyped exp typ -> case checkExp env typ exp of
                      --                  Ok _ -> Ok typ
                        --                Bad s -> Bad s

isBad :: Err a -> Bool
isBad (Bad _) = True
isBad _ = False

isValidArgs :: [Type] -> [Type] -> Bool
isValidArgs (typ1:[]) (typ2:[]) = isValidAss typ1 typ2
isValidArgs (typ1:xs) (typ2:ys) = if isValidAss typ1 typ2 then isValidArgs xs ys else False

isValidAss :: Type -> Type -> Bool
isValidAss TDouble TInt = True
isValidAss typ1 typ2 = typ1 == typ2

isValidType :: Type -> Type -> Bool
isValidType TInt TDouble = True
isValidType TDouble TInt = True
isValidType typ1 typ2 = typ1 == typ2 && typ1 /= TVoid

inferBin :: Env -> Exp -> Exp -> Err Type
inferBin env exp1 exp2 = case inferExp env exp1 of
                            Ok (TInt,_) -> case inferExp env exp2 of
                                Ok (TInt,_) -> Ok TInt
                                Ok (TDouble,_) -> Ok TDouble
                                _ -> Bad "Bad type"
                            Ok (TDouble, _) -> case inferExp env exp2 of
                                Ok (TInt, _) -> Ok TDouble
                                Ok (TDouble,_) -> Ok TDouble
                                _ -> Bad "Bad type"
                            _ -> Bad "Bad type"

checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
    (expTyp,_) <- inferExp env exp
    if expTyp == typ then
        return ()
        else
        Bad $ "type of " ++ printTree exp ++
               "expected " ++ printTree typ ++
               "but found " ++ printTree expTyp




checkStm :: Type -> Env -> Stm -> Err (Env, A.Stm)
checkStm retTyp env stm =
                    case stm of 
                        SExp exp -> do
                            (typ,exp') <- inferExp env exp
                            
                            return (env, A.SExp typ exp')
                        SDecls typ ids -> do
                                env' <- foldM (\newEnv id -> updateVar newEnv id typ) env ids
                                return (env', A.SDecls typ ids)               
                        SInit typ id exp -> do
                            newEnv <- updateVar env id typ
                            (expTyp,exp') <- inferExp newEnv exp
                            if isValidAss typ expTyp then return (newEnv, A.SInit typ id exp')
                            else Bad "Types doesn't match"
                        SRet exp -> do
                            (typ,exp') <- inferExp env exp
                            if typ == retTyp then return (env, A.SRet typ exp')
                            else Bad "Return type does not match"
                        SWhile exp stm' -> do
                                (_, exp' ) <- inferExp env exp
                                let newEnv = newBlock env
                                checkExp newEnv TBool exp
                                (env', stm'') <- checkStm retTyp newEnv stm'
                                
                                -- maybe env'
                                return (env, A.SWhile exp' stm'')
                        SIf exp stm1 stm2 -> do 
                                    (_, exp') <- inferExp env exp
                                    let newEnv = newBlock env
                                    checkExp env TBool exp
                                    (env', stm1')<-  checkStm retTyp newEnv stm1
                                    (env'',stm2') <- checkStm retTyp newEnv stm2
                                     -- maybe env'' (?)
                                    return (env, A.SIf exp' stm1' stm2')
                        SBlock stmxs -> do
                                    let newEnv = newBlock env
                                    (env, stms') <- checkStms retTyp (newBlock env) stmxs
                                    return (env, A.SBlock stms')

-- transformExp :: Exp -> A.Exp
-- transformExp exp = 

checkStms :: Type -> Env ->  [Stm] -> Err (Env, [A.Stm])
checkStms typ env [] = return (env, [])
checkStms typ env (stm:stms) = do 
                        (env', astm) <- checkStm typ env stm
                        (env'', astms) <- checkStms typ env' stms
                        return (env'', astm:astms)

checkDef :: Env -> Func -> Err (Env, A.Func) 
checkDef env func@(FDef typ id (FArgument args) (FBody stms)) = do
                    newEnv <- foldM (\env (FArgs typ id) -> updateVar env id typ) (newBlock env) args
                    (newEnv', astms') <- checkStms typ newEnv stms                              
                    return (newEnv', A.DFun typ id args astms')

getTypes :: Func -> (Id, ([Type],Type))
getTypes (FDef typ id (FArgument args) body) = (id, ( (map (\(FArgs typ _) -> typ) args ),typ))

lookupVar :: Env -> Id -> Err Type
lookupVar (_, []) (Id id) = Bad $  "Variable does not exist " ++ printTree id
lookupVar env@(sig,top:stack) id = case Map.lookup id top of
                                    Nothing -> lookupVar (sig,stack) id
                                    Just typ -> return typ           

lookupFun :: Env -> Id -> Err ([Type],Type)
lookupFun (sig, _) id = 
            case Map.lookup id sig of
                Just pair -> Ok pair
                Nothing -> Bad "No function found"

updateVar :: Env -> Id -> Type -> Err Env
updateVar _ _ TVoid = Bad "Can not assign variable as void"
updateVar (sig, top:stack) id typ = 
            case Map.lookup id top of
                Nothing-> do
                    let newStack = Map.insert id typ top 
                    Ok (sig, newStack:stack)
                Just id -> Bad $ "Already in environment" ++ printTree id
                --Ok _ -> Ok (sig, top:stack)

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun env@(sig, stack) id funcTypes = 
            case lookupFun env id of
                Bad _ -> Ok (Map.insert id funcTypes sig, stack)
                Ok _ -> Bad $ "Function already exists " ++ printTree id

newBlock :: Env -> Env
newBlock (sig, stack) = (sig, Map.empty:stack)

emptyEnv :: Env
emptyEnv = (Map.fromList builtIn, [Map.empty])
