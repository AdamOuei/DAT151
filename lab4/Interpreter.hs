-- --| Interpreter for lambda-calculus with if, +, -, <.
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
import Debug.Trace

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
    let sig = foldl makeThisWork Map.empty defs
    let cxt = (Cxt {cxtSig = sig, cxtEnv = Map.empty, cxtStrategy = strategy})
    eval cxt mainExp >>= convertToInt
    where 
        makeThisWork :: Sig -> Def -> Sig
        makeThisWork mapp def@(DDef ident ids exp) = Map.insert ident (makeShit ids exp) mapp
        makeShit :: [Ident] -> Exp -> Exp
        makeShit xs exp = foldr EAbs exp xs

eval :: Cxt -> Exp -> Err Value
eval cxt = \case

      EInt int -> return $ VInt int

      EVar id ->
          
          case Map.lookup id (cxtEnv cxt) of
            Just entry -> entryValue cxt entry
            Nothing -> case Map.lookup id (cxtSig cxt) of
              Just e -> eval (cxt{cxtEnv = Map.empty}) e
              Nothing -> throwError $ "unbound identifier" ++ printTree id

      EAbs id exp -> return $ VFun id exp (cxtEnv cxt)
      
      EApp f a-> do
        v1 <- eval cxt f
        case cxtStrategy cxt of
          CallByValue -> do 
            v2 <- eval cxt a
            case v1 of
              VInt{} -> throwError "Can not apply an integer"
              VFun x e env -> eval (cxt {cxtEnv = Map.insert x (Val v2) env }) e
          CallByName -> 
            case v1 of
              VInt{} -> throwError "Can not apply an integer"
              VFun x e env -> do
                let entry = Clos a (cxtEnv cxt)
                eval (cxt {cxtEnv = Map.insert x entry env}) e

      EAdd exp exp' -> do
        a <- eval cxt exp >>= convertToInt 
        b <- eval cxt exp' >>= convertToInt 

        return $ VInt (a+b)
      ESub exp exp' -> do
        a <- eval cxt exp >>= convertToInt 
        b <- eval cxt exp' >>= convertToInt 

        return $ VInt (a-b)
      ELt exp exp'  -> do
        a <- eval cxt exp >>= convertToInt 
        b <- eval cxt exp' >>= convertToInt 

        if a < b then return $ VInt 1 else return $ VInt 0 
      EIf cond thenExp elseExp -> do
         u <- eval cxt cond >>= convertToInt
         case u of
          1 -> eval cxt thenExp
          0 -> eval cxt elseExp
          _ -> throwError "Not valid"


          
convertToInt :: Value -> Err Integer
convertToInt (VInt a ) = return a
convertToInt _ = throwError "Not an integer"
        

entryValue :: Cxt -> Entry -> Err Value
entryValue cxt  = \case
          Val val-> return val
          Clos exp env -> eval (cxt{cxtEnv = env}) exp
               

-- updateCxt :: Cxt -> Ident -> Entry -> Err Cxt
-- updateCxt cxt id entry = return cxt{cxtEnv = Map.insert id entry (cxtEnv cxt), cxtSig =  cxtSig cxt}