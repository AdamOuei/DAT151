-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

{-# LANGUAGE LambdaCase #-}

module Interpreter (interpret, Strategy(..)) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Fun.Abs
import Fun.Print
import Fun.ErrM

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Error monad.

--type Err = Except String

data Cxt = Cxt { cxtStrategy :: Strategy, cxtSig :: Sig, cxtEnv :: Env}

data Value = VInt Integer | VFun Ident Exp Env

type Sig = Map Ident Exp

type Env = Map Ident Entry

data Entry = Val Value | Clos Exp Env

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  throwError $ "TODO: implement interpreter"


eval :: Cxt -> Exp -> Err Value
eval cxt = \case

      EInt int -> return $ VInt int

      EVar id -> 
          case Map.lookup id (cxtEnv cxt) of
            Just entry -> return $ entryValue cxt entry
            Nothing -> Bad $ "unbound identifier" ++ printTree id

      EAbs id exp -> return $ VFun id exp (cxtEnv cxt)
      
      EApp f a-> do
        v1 <- eval cxt f
        case cxtStrategy cxt of
          CallByValue -> do 
            v2 <- eval cxt a
            case v1 of
              VInt{} -> Bad $ "Can not apply an integer"
              VFun x e env -> eval (cxt {cxtEnv = Map.insert x (Val v2) env }) e
          CallByName -> 
            case v1 of
              VInt{} -> Bad $ "Can not apply an integer"
              VFun x e env -> do
                let entry = Clos a (cxtEnv cxt)
                eval (cxt {cxtEnv = Map.insert x entry env}) e

      EAdd exp exp' -> do
        (VInt a) <- eval cxt exp
        (VInt b) <- eval cxt exp'
        
        return $ VInt (a+b)
      ESub exp exp' -> do
        (VInt a) <- eval cxt exp
        (VInt b) <- eval cxt exp'
        
        return $ VInt (a-b)
      ELt exp exp'  -> do
        a <- eval cxt exp
        b <- eval cxt exp'
        if a < b then return $ VInt 1 else return $ VInt 0 
      EIf cond thenExp elseExp -> do
        u <- eval cxt cond 
        case u of
          VInt 1 -> eval cxt thenExp
          VInt 0 -> eval cxt elseExp
          _ -> Bad $ "Not valid" 
        


entryValue :: Cxt -> Entry -> Value
entryValue cxt  = \case
          Val val-> val
          Clos exp env -> do 
            v <- eval cxt exp
            return v


--lookupVar :: Id -> Env -> Value 


--updateEnv :: Env -> Id -> Value -> Env