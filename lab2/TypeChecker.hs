module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM


typecheck :: Program -> Err ()
typecheck p = return ()

type Env = (Sig,[Context]) -- functions and context stack
type Sig = Map Id ([Type],Type) -- function type signature
type Context = Map Id Type -- variables with their types

lookupVar :: Env -> Id -> Err Type
lookupVar (_, stack) id = 
          case catMaybes maybeList of
            [] -> Bad "No variable found"
            [Just x:_] -> Ok x
            where maybeList = map lookup id stack
            

lookupFun :: Env -> Id -> Err ([Type],Type)
lookupFun (sig, _) id = 
            case lookup id sig of
                Nothing -> Bad "No function found"
                Just pair -> Ok pair

updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, stack) 

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
newBlock :: Env -> Env
emptyEnv :: Env