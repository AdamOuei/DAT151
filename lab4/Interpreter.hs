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

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Error monad.

type Err = Except String

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

      EInt i -> return $ VInt i

      EVar x -> do
          case Map.lookup x (ctxEnv ctx) of
            Just entry -> return $ entryValue cxt entry
            Nothing -> Bad $ "unbound identifier" ++ printTree x

      EAbs x e -> return $ VFun  x e (ctxEnv ctx)
      
      EApp f a-> do
        v1 <- eval cxt f
        case cxtStrategy cxt of
          CallByValue -> do 
            v2 <- eval cxt a
            case v1 of
              VInt{} -> Bad $ "Can not apply an integer"
              VFun x e env -> eval (cxt {cxtEnv = Map.insert x (Val v2) env }) e
          CallByName -> do
            case v1 of
              VInt{} -> Bad $ "Can not apply an integer"
              VFun x e env -> do
                let entry = Clos a (cxtEnv cxt)
                eval (cxt {cxtEnv = Map.insert x entry env}) e


        

      


      EAdd e e' = todo
      ESub e e' = todo
      ELt e e'  = todo
      EIf c t e = todo