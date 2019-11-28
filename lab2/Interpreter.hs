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

eval ::  Env -> Exp -> IO (Val, Env)

execProg :: Env -> IO ()

execStm :: Env -> Stm -> IO (Maybe Val, Env)

lookupVar :: Env -> Id -> IO Val

lookupFun :: Env -> Id -> IO Func

exitBlock :: Env -> Env

newBlock :: Env -> Env

emptyEnv :: Env








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







