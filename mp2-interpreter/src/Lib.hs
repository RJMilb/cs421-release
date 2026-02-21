module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = typeVal (H.lookup s env)
    where 
        typeVal :: Maybe Val -> Val
        typeVal Nothing = ExnVal "No match in env"
        typeVal (Just v) = v


--- ### Arithmetic

eval (IntOpExp op e1 e2) env 
    | op == "/" && yreal y == 0 = ExnVal "Division by 0"
    | otherwise = intOpExpHelp (H.lookup op intOps) x y
    where
        x = eval e1 env
        y = eval e2 env
        yreal (IntVal i) = i
        yreal _ = 1
        intOpExpHelp :: Maybe (Int -> Int -> Int) -> Val -> Val -> Val
        intOpExpHelp Nothing _ _ = ExnVal "No match in intOps"
        intOpExpHelp (Just o) a b = liftIntOp o a b


--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = boolOpExpHelp (H.lookup op boolOps) (eval e1 env) (eval e2 env)
    where
        boolOpExpHelp :: Maybe (Bool -> Bool -> Bool) -> Val -> Val -> Val
        boolOpExpHelp Nothing _ _ = ExnVal "No match in boolOps"
        boolOpExpHelp (Just o) x y = liftBoolOp o x y

eval (CompOpExp op e1 e2) env = compOpExpHelp (H.lookup op compOps) (eval e1 env) (eval e2 env)
    where
        compOpExpHelp :: Maybe (Int -> Int -> Bool) -> Val -> Val -> Val
        compOpExpHelp Nothing _ _ = ExnVal "No match in compOps"
        compOpExpHelp (Just o) x y = liftCompOp o x y

--- ### If Expressions

eval (IfExp e1 e2 e3) env = ifExpHelp (eval e1 env) e2 e3
    where
        ifExpHelp :: Val -> Exp -> Exp -> Val
        ifExpHelp (BoolVal b) ex1 ex2 = if b
                                        then eval ex1 env
                                        else eval ex2 env
        ifExpHelp _ _ _ = ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = appExpHelp (eval e1 env) 
    where
        appExpHelp :: Val -> Val 
        appExpHelp (CloVal params body cenv) = eval body (H.union (H.fromList (add2Env params args)) cenv)
        appExpHelp _ = ExnVal "Apply to non-closure"
        add2Env :: [String] -> [Exp] -> [(String, Val)] 
        add2Env (x:xs) (y:ys) = (x, eval y env ) : add2Env xs ys
        add2Env _ [] = []
        add2Env [] _ = []

--- ### Let Expressions

eval (LetExp pairs body) env = eval body (H.fromList (letExpHelp pairs))
    where
        letExpHelp :: [(String, Exp)] -> [(String, Val)]
        letExpHelp ((v, e):xs) = (v, eval e env) : letExpHelp xs
        letExpHelp [] = []


--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, H.insert var (eval e env) env )

--- ### Sequencing

exec (SeqStmt seq) penv env = 
    foldl step ("", penv, env) seq
  where
    step (accLog, p, e) stmt = 
        let (newLog, newP, newE) = exec stmt p e
        in (accLog ++ newLog, newP, newE)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = ifStmtHelp (eval e1 env) s1 s2
    where
        ifStmtHelp :: Val -> Stmt -> Stmt -> Result
        ifStmtHelp (BoolVal b) st1 st2 = if b
                                        then exec st1 penv env
                                        else exec st2 penv env
        ifStmtHelp _ _ _ = ( "exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env = callStmtHelp (H.lookup name penv) 
    where 
        callStmtHelp :: Maybe Stmt  -> Result 
        callStmtHelp Nothing = ("Procedure " ++ name ++ " undefined", penv, env)
        callStmtHelp ( Just (ProcedureStmt _ params body)) = exec body penv (H.union (H.fromList (add2Env params args)) env)
        callStmtHelp _ = ("exn: Invalid procedure definition", penv, env)
        add2Env :: [String] -> [Exp] -> [(String, Val)] 
        add2Env (x:xs) (y:ys) = (x, eval y env ) : add2Env xs ys
        add2Env _ [] = []
        add2Env [] _ = []

-- ProcedureStmt String [String] Stmt
-- CallStmt String [Exp]