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
inferExp env exp =
            case exp of
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
                                                
                EDec id -> 
                EInc2 id ->
                EDec2 id ->
                EMul exp exp ->
                EDiv exp exp ->
                EAdd exp exp ->
                ESub exp exp ->
                ELess exp exp ->
                EGre exp exp ->
                ELeq exp exp ->
                EGeq exp exp ->
                EEqua exp exp ->
                EIneq exp exp ->
                EConj exp exp ->
                EDisj exp exp ->
                EAss id exp ->
                ETyped exp typ ->


checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
    typ2 <- inferExp env exp
    if (typ2 == typ) then
        return ()
        else
        fail $ "type of " ++ printTree exp ++
               "expected " ++ printTree typ ++
               "but found " ++ printTree typ2

checkStms :: Env -> Stms -> Err ()

checkDef :: Env -> Def -> Err ()

checkProg :: Program -> Err ()

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
