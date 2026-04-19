{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

-- ### Evaluation helpers

tag1 = 39236
tag2 = 97713

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (Pair c (Pair e Nil)) = liftA2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (Pair c (Pair e Nil)) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

-- Evaluates a value representing a list into an actual list
getList :: Val -> EvalState [Val]
getList Nil = return []
getList (Pair v1 v2) =
  do xs <- getList v2
     return (v1 : xs)
getList e = throwError $ InvalidSpecialForm "special" e

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
eval v@(Number _) = return v --unimplemented "Evaluating numbers"
eval v@(Boolean _) = return v --unimplemented "Evaluating booleans"

-- Symbol evaluates to the value bound to it
eval (Symbol sym) = do
  env <- get 
  case H.lookup sym env of 
    Just val -> return val
    Nothing -> throwError $ UndefSymbolError sym


-- Function closure is also self-evaluating
eval v@(Func _ _ _) = return v

-- We check to see if the pair is a "proper list". If it is,
-- then we try to evaluate it, as one of the following forms:
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(Pair v1 v2) = case flattenList expr of
  Left _ -> throwError $ InvalidExpression expr
  Right lst -> evalList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    -- TODO
    evalList [Symbol "quote", e] = return e

    -- unquote (illegal at surface evaluation)
    -- TODO: since surface-level `unquote` is illegal, all you need to do is
    -- to throw a diagnostic
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote expr

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (Pair (Symbol "unquote") v) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (Pair (Symbol "unquote") (Pair v Nil)) = eval v
      evalQuasi n (Pair (Symbol "quasiquote") (Pair v Nil)) =
        do v' <- evalQuasi (n+1) v
           return $ Pair (Symbol "quasiquote") (Pair v' Nil)
      evalQuasi n (Pair (Symbol "unquote") (Pair v Nil)) =
        do v' <- evalQuasi (n-1) v
           return $ Pair (Symbol "unquote") (Pair v' Nil)
      evalQuasi n (Pair x y) = Pair <$> evalQuasi n x <*> evalQuasi n y
      evalQuasi _ v = return v

    -- cond
    -- TODO: Handle `cond` here. Use pattern matching to match the syntax
    evalList (Symbol "cond" : clauses) =
      case clauses of
        [] -> throwError $ InvalidSpecialForm "cond" expr
        _  -> condHelp clauses
      where 
        condHelp [] = return Void
        condHelp (clause:cs) = do
          (c, e) <- getListOf2 clause
          case c of
            Symbol "else" -> 
              if null cs 
                then eval e 
                else throwError $ InvalidSpecialForm "cond" expr
            _ -> do
              condition <- eval c
              case condition of
                Boolean False -> condHelp cs 
                _             -> eval e       

    -- let
    -- TODO: Handle `let` here. Use pattern matching to match the syntax
    evalList [Symbol "let", bindings, body] = do 
      oldEnv <- get
      bindingList <- getList bindings
      newBindings <- mapM getBinding bindingList
      let envForBody = H.union (H.fromList newBindings) oldEnv
      put envForBody
      res <- eval body
      put oldEnv
      return res

    -- lambda
    -- TODO: Handle `lambda` here. Use pattern matching to match the syntax
    evalList [Symbol "lambda", args, body] = do -- ???
      env <- get 
      argList <- getList args 
      params <- mapM getSym argList
      return $ Func params body env 

    -- define function
    evalList [Symbol "define", Pair (Symbol fname) args, body] =
      do env <- get
         argList <- getList args
         val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
         modify $ H.insert fname val
         return Void

    -- define variable
    evalList [Symbol "define", Symbol var, exp] = do
      --env <- get
      val <- eval exp
      modify $ H.insert var val
      return Void

    -- define-macro
    -- TODO: Handle `define-macro` here. Use pattern matching to match
    -- the syntax
    evalList [Symbol "define-macro", Pair (Symbol mname) args, body] = do 
      argList <- getList args
      params <- mapM getSym argList 
      let macro = Macro params body
      modify $ H.insert mname macro
      return Void

    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) =
      do f <- eval fexpr
         apply f args

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
  -- Function
apply (Func params body cEnv) args = do 
  argVals <- mapM eval args
  oldEnv <- get 
  let newEnv = H.union (H.fromList (zip params argVals)) (H.union cEnv oldEnv)
  put newEnv 
  result <- eval body 
  put oldEnv
  return result

  -- Macro
    -- TODO: implement macro evaluation
    -- Use do-notation!
apply (Macro params body) args = do 
  oldEnv <- get 
  let newEnv = H.union (H.fromList (zip params args)) oldEnv
  put newEnv 
  mac <- eval body 
  put oldEnv
  result <- eval mac 
  return result

  -- Primitive
apply (PrimFunc p) args =
  do argVals <- mapM eval args
     p argVals
  -- Other values are not applicable
apply f args = throwError $ CannotApply f args
