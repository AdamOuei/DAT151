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
                    EInt Integer -> Ok Integer
                    EDouble Double -> Ok Double
                    ETrue -> Ok Bool
                    EFalse -> Ok Bool
                    EId id -> lookupVar env id
                    ECall id argExps -> do
                                    case lookupFun env id of
                                        Bad s -> Bad s
                                        Ok (argsTypes, typ) -> do
                                                            let argsCheck = zipWith (\x y -> checkExp env x y) argTypes argExps
                                                            case any isBad argsCheck of
                                                                True -> Bad "Arguments not ok"
                                                                False -> Ok typ
                -- Maybe change to TBool and add void
                    EInc id -> case lookupVar env id of
                                Ok Bool -> Bad "Not valid type"
                                _ -> lookupVar env id
                    EDec id ->  case lookupVar env id of
                                Ok Bool -> Bad "Not valid type"
                                _ -> lookupVar env id
                    EInc2 id ->  case lookupVar env id of
                                Ok Bool -> Bad "Not valid type"
                                _ -> lookupVar env id
                    EDec2 id ->  case lookupVar env id of
                                Ok Bool -> Bad "Not valid type"
                                _ -> lookupVar env id
                    EMul exp1 exp2 -> inferBin env exp1 exp2
                    EDiv exp1 exp2 -> inferBin env exp1 exp2
                    EAdd exp1 exp2 -> inferBin env exp1 exp2
                    ESub exp1 exp2 -> inferBin env exp1 exp2
                    ELess exp1 exp2 -> case inferBin env exp1 exp2 of
                                        Ok _ -> Ok Bool
                                        _ -> Bad "Bad type"                
                    EGre exp1 exp2 -> case inferBin env exp1 exp2 of
                                        Ok _ -> Ok Bool
                                        _ -> Bad "Bad type"
                    ELeq exp1 exp2 -> case infeBin env exp1 exp2 of
                                        Ok _ -> Ok Bool
                                        _ -> Bad "Bad type"
                    EGeq exp1 exp2 -> case inferBin env exp1 exp2 of
                                        Ok _ -> Ok Bool
                                        _ -> Bad "Bad type"
                    EEqua exp1 exp2 -> case inferExp env exp1 of
                                        Ok Bool -> case inferExp env exp2 of
                                                    Ok Bool -> Ok Bool
                                                    _ -> Bad "Bad type"
                                        Bad _ -> Bad "Bad type"
                                        _ -> case inferExp env exp2 of
                                                    Ok Bool -> bad "Bad type"
                                                    Bad _ -> Bad "Bad type"
                                                    _ -> Ok Bool
                    EIneq exp1 exp2 -> case inferExp env exp1 of
                                        Ok Bool -> case inferExp env exp2 of
                                                    Ok Bool -> Ok Bool
                                                    _ -> Bad "Bad type"
                                        Bad _ -> Bad "Bad type"
                                        _ -> case inferExp env exp2 of
                                                    Ok Bool -> bad "Bad type"
                                                    Bad _ -> Bad "Bad type"
                                                    _ -> Ok Bool
                    EConj exp exp -> case inferExp env exp1 of
                                        Ok Bool -> case inferExp env exp2 of
                                                    Ok Bool -> Ok Bool
                                                    _ -> Bad "Wrong type"
                                        Bad _ -> "Not valid type"
                    EDisj exp exp -> case inferExp env exp1 of
                                        Ok Bool -> case inferExp env exp2 of
                                                    Ok Bool -> Ok Bool
                                                    _ -> Bad "Wrong type"
                                        Bad _ -> "Not valid type"
                    EAss id exp -> case lookupVar env id of
                                    Bad s -> Bad s
                                    Ok typ -> case checkExp env typ exp of
                                                Bad s -> Bad s
                                                Ok _ -> Ok typ 
                    ETyped exp typ -> case checkExp env typ exp of
                                        Ok _ -> Ok typ
                                        Bad s -> Bad s

isBad :: Err a -> Bool
isBad Bad _ = True
isBad _ = False

inferBin :: Env -> Exp -> Exp -> Err Type
inferBin env exp1 exp2 = case inferExp env exp1 of
                            Ok Integer -> case inferExp env exp2 of
                                Ok Integer -> Ok Integer
                                Ok Double -> Ok Double
                                _ -> Bad "Bad type"
                            Ok Double -> case inferExp env exp2 of
                                Ok Bool -> Bad "Bad type"
                                Bad x -> Bad x
                                _ -> Ok Double
                            _ -> Bad "Bad type"


-- do
--                     type1 = inferExp env exp1
--                     type2 = inferExp env exp2
--                     if type1 `elem` types
--                         then
--                             if type2 `elem` types
--                                 then
--                             case checkExp env type1 exp2 of
--                                 Bad s -> Bad s
--                                 _ -> 
--                         else 
--                             Bad "Not a binary type"

checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
    typ2 <- inferExp env exp
    if (typ2 == typ) then
        return ()
        else
        Bad $ "type of " ++ printTree exp ++
               "expected " ++ printTree typ ++
               "but found " ++ printTree typ2

checkStms :: Env -> Type -> Stms -> Err Env
checkStms env typ stms = case stms of 
                        SExp exp -> case inferExp env exp of
                                        Ok _ -> return Ok env
                                        _ -> Bad "Statement with faulty types"
                        -- Check line below, map not correct
                        SDecls typ ids -> map updateVar env ids typ
                        SInit typ id exp -> case checkExp env typ exp of
                                            Ok _ -> updateVar env id typ
                                            Bad s -> Bad s
                        SRet exp -> 
                        SWhile exp stm -> case checkExp env Bool exp of
                                            Ok _ -> checkStms env stm
                                            Bad s -> Bad s
                        SIf exp stm1 stm2 -> case checkExp env Bool exp of
                                            Ok _ -> case checkStms env stm1 of
                                                        Ok _ -> checkStms env stm2
                                                        Bad s -> Bad s
                                            Bad s -> Bad s
                        SBlock stmxs -> map (checkStms env) stmxs

checkDef :: Env -> Def -> Err ()
checkDef env def = 

checkProg :: Program -> Err ()
checkProg prog = map checkDef

lookupVar :: Env -> Id -> Err Type
lookupVar (_, []) id = Bad "Variable does not exist"
lookupVar (sig, top:stack) id   | isNothing(Map.lookup id top) = lookupVar (sig, stack) id 
                                | otherwise = Ok $ fromJust $ Map.lookup id top

-- lookupVar (_, stack) id = 
--           case catMaybes maybeList of
--             [] -> Bad "No variable found"
--             [Just boomer:_] -> Ok boomer
--             where maybeList = map Map.lookup id stack
            

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
