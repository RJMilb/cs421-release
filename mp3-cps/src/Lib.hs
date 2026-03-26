--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 con = con 1
factk n con = factk (n - 1) (\res -> con (n * res))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] ek ok 
    | even x    = ek x 
    | otherwise = ok x 
evenoddk (x:xs) ek ok 
    | even x    = evenoddk xs (\res -> ek (res + x)) ok
    | otherwise = evenoddk xs ek (\res -> ok (res + x))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp i) = True 
isSimple (VarExp v) = True 
isSimple (LamExp s e) = True
isSimple (IfExp eif et ef) = isSimple eif && isSimple et && isSimple ef 
isSimple (OpExp op e1 e2) = isSimple e1 && isSimple e2
isSimple (AppExp f e) = False

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
-- cpsExp = undefined
-- cpsExp (LamExp s e) k n = undefined
--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)
--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k n 
    | isSimple e = (AppExp (AppExp f e) k, n)
    | otherwise = 
        let (v, n1) = gensym n
            con = LamExp v (AppExp (AppExp f (VarExp v)) k)
        in cpsExp e con n1
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n 
    | isSimple e1 && isSimple e2 = (AppExp k (OpExp op e1 e2), n)
    | not (isSimple e1) && isSimple e2 = 
        let (v, n1) = gensym n
            con = LamExp v (AppExp k (OpExp op (VarExp v) e2))
        in cpsExp e1 con n1
    | isSimple e1 && not (isSimple e2) = 
        let (v, n1) = gensym n
            con = LamExp v (AppExp k (OpExp op e1 (VarExp v)))
        in cpsExp e2 con n1
    | otherwise = 
        let (v1, n1) = gensym n
            (v2, n2) = gensym n1 
            inCon = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
            (translatedE, n3) = cpsExp e2 inCon n2 
            outCon = LamExp v1 translatedE
        in cpsExp e1 outCon n3

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp eif et ef) k n 
    | isSimple eif = 
        let (t1, n1) = cpsExp et k n
            (f1, n2) = cpsExp ef k n1 
        in (IfExp eif t1 f1 ,n2)
    | otherwise = 
        let (v, n1) = gensym n
            (t1, n2) = cpsExp et k n1
            (f1, n3) = cpsExp ef k n2 
            con = LamExp v (IfExp (VarExp v) t1 f1)
        in cpsExp eif con n3
--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl s xs e) = 
    let (ek, n) = cpsExp e (VarExp "k") 0
    in Decl s (xs ++ ["k"]) ek
