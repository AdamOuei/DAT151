module TypeChecker where

import Control.Monad

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM


typecheck :: Program -> Err ()
typecheck p = return ()

type Env = (Sig,[Context]) -- functions and context stack
type Sig = Map Id ([Type],Type) -- function type signature
type Context = Map Id Type -- variables with their types

inferExp :: Env -> Exp -> Err Type
inferExp env exp = case exp of
                    EInt int -> Ok TInt
                    EDouble doub -> Ok TDouble
                    ETrue -> Ok TBool
                    EFalse -> Ok TBool
                    EId id -> lookupVar env id
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

isBad :: Err a -> Bool
isBad (Bad _) = True
isBad _ = False

inferBin :: Env -> Exp -> Exp -> Err Type
inferBin env exp1 exp2 = case inferExp env exp1 of
                            Ok TInt -> case inferExp env exp2 of
                                Ok TInt -> Ok TInt
                                Ok TDouble -> Ok TDouble
                                _ -> Bad "Bad type"
                            Ok TDouble -> case inferExp env exp2 of
                                Ok TBool -> Bad "Bad type"
                                Bad x -> Bad x
                                _ -> Ok TDouble
                            _ -> Bad "Bad type"

checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
    typ2 <- inferExp env exp
    if typ2 == typ then
        return ()
        else
        Bad $ "type of " ++ printTree exp ++
               "expected " ++ printTree typ ++
               "but found " ++ printTree typ2




checkStm :: Type -> Env -> Stm -> Err Env
checkStm retTyp env stm = case stm of 
                        SExp exp -> case inferExp env exp of
                                        Ok _ -> Ok env
                                        _ -> Bad "Statement with faulty types"
                        SDecls typ (id:ids) -> do
                            if typ == TVoid then Bad "Tried to declare a variable with Void"
                            else do
                                newEnv <- updateVar env id typ
                                checkStm  retTyp env $ SDecls typ ids

                        SDecls typ [] -> return env
                        SInit typ id exp -> case checkExp env typ exp of
                                            Ok _ -> updateVar env id typ
                                            Bad s -> Bad s
                        SRet exp -> case checkExp env retTyp exp of 
                                        Ok _ -> Ok env
                                        _ -> Bad "Not okay return statement"
                        SWhile exp stm -> case checkExp env TBool exp of
                                            Ok _ -> checkStm  retTyp env stm
                                            Bad s -> Bad s
                        SIf exp stm1 stm2 -> case checkExp env TBool exp of
                                            Ok _ -> case checkStm  retTyp env stm1 of
                                                        Ok _ -> checkStm  retTyp env stm2
                                                        Bad s -> Bad s
                                            Bad s -> Bad s
                        SBlock stmxs -> do
                                    checkStms retTyp (newBlock env) stmxs
                                    return env


checkStms :: Type -> Env ->  [Stm] -> Err Env
checkStms retTyp = foldM (checkStm retTyp)

checkDef :: Env -> Func -> Err Env
checkDef env (FDef typ id args body) = case updateFun env id (getTypes args, typ) of
                                    Ok env2 -> case map (checkStm env2) typ body of
                                                    Ok env3 -> Ok env3
                                                    Bad _ -> Bad "Could not parse body"
                                    Bad _ -> Bad "Wrong Function head"

getTypes :: Argument -> [Type]
getTypes (FArgument []) = []
getTypes (FArgument FArgs typ id : xs) = typ:getTypes (FArgument xs)

checkProg :: Program -> Err ()
checkProg (Prg prog) = do 
                    let envs = map (checkDef emptyEnv) prog
                    if any isBad envs then Bad "Faulty Program" else return ()

lookupVar :: Env -> Id -> Err Type
lookupVar (_, []) id = Bad "Variable does not exist"
lookupVar (sig, top:stack) id   | isNothing(Map.lookup id top) = lookupVar (sig, stack) id 
                                | otherwise = Ok $ fromJust $ Map.lookup id top          

lookupFun :: Env -> Id -> Err ([Type],Type)
lookupFun (sig, _) id = 
            case Map.lookup id sig of
                Nothing -> Bad "No function found"
                Just pair -> Ok pair

updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, top:stack) id typ = 
            case lookupVar (sig, top:stack) id of
                Bad _ -> do
                    let newStack = Map.insert id typ top 
                    Ok (sig, newStack:stack)
                Ok _ -> Bad "Already in environment"
                --Ok _ -> Ok (sig, top:stack)

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig, stack) id funcTypes = 
            case lookupFun (sig, stack) id of
                Bad _ -> Ok (Map.insert id funcTypes sig, stack)
                Ok _ -> Bad "Function already exists"

newBlock :: Env -> Env
newBlock (sig, stack) = (sig, Map.empty:stack)

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])
