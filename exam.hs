module Exam where

import Data.List (intersect)

data Exp = Val Int | Var String | Not Exp | And Exp Exp | Or Exp Exp | Impl Exp Exp | ForAll String Exp | Exists String Exp deriving (Eq)
data Tree = Branch (String) (Tree) (Tree) | LonelyBranch (String) (Tree) | Leaf (String) deriving (Eq, Show)

-- output
instance Show Exp where
	show (Val a) = show a
	show (Var x) = x
	show (Not e) = "(!" ++ show e ++ ")"
	show (And e1 e2) = "(" ++ show e1 ++ " & " ++ show e2 ++ ")"
	show (Or e1 e2) = "(" ++ show e1 ++ " v " ++ show e2 ++ ")"
	show (Impl e1 e2) = "(" ++ show e1 ++ " -> " ++ show e2 ++ ")"
	show (ForAll x e) = "V" ++ x ++ show e
	show (Exists x e) = "E" ++ x ++ show e

-- start point
eval :: Exp -> Bool
eval exp = eval' exp []

eval' :: Exp -> [(String, Int)] -> Bool
eval' (ForAll x e) vMap = eval' e ((x, 0):vMap) && eval' e ((x, 1):vMap)
eval' (Exists x e) vMap = eval' e ((x, 0):vMap) || eval' e ((x, 1):vMap)	
eval' e  vMap = case calc e vMap of 
	(Val 1) -> True
	(Val 0) -> False 
	e -> getResult $ check [] [e]

calc :: Exp -> [(String, Int)] -> Exp
calc (Not e) m = combine $ Not (calc e m)
calc (And e1 e2) m = combine $ And (calc e1 m) (calc e2 m)
calc (Or e1 e2) m = combine $ Or (calc e1 m) (calc e2 m)
calc (Impl e1 e2) m = combine $ Or (calc (Not e1) m) (calc e2 m)
calc a@(Var v) m = case lookup v m of
		Just x -> Val x
		Nothing -> a
		
-- basis equations
combine :: Exp -> Exp
combine (Not (Val 1)) = Val 0
combine (Not (Val 0)) = Val 1

combine (Or (Val 1) _) = Val 1
combine (Or _ (Val 1)) = Val 1
combine (Or e (Val 0)) = e
combine (Or (Val 0) e) = e

combine (And (Val 1) e) = e
combine (And e (Val 1)) = e
combine (And _ (Val 0)) = Val 0
combine (And (Val 0) _) = Val 0
combine e = e

-- исчисление высказываний
isVar :: Exp -> Bool
isVar (Var x) = True
isVar _ = False

check :: [Exp] -> [Exp] -> Tree
check [] [] = Leaf "TQBF"
check l r 
	| intersect l r /= [] = Leaf "TQBF"
	| esLeft /= [] = ltSwapper (head esLeft) l r
	| esRight /= [] = rfSwapper (head esRight) l r
	| otherwise = Leaf "FAIL"
	where 
		esLeft = filter (\x -> not $ isVar x) l
		esRight = filter (\x -> not $ isVar x) r

ltSwapper :: Exp -> [Exp] -> [Exp] -> Tree
ltSwapper (Not x) l r   = LonelyBranch "|l not|" (check (l `exclude` (Not x)) (x:r)) 
ltSwapper (And x y) l r = LonelyBranch "|l and|" (check (x : y : (l `exclude` (And x y))) r) 
ltSwapper (Or x y) l r = Branch "|l or|" (check (x : (l `exclude` (Or x y))) r) (check (y : (l `exclude` (Or x y))) r) 
ltSwapper (Impl x y) l r = Branch "|l impl|" (check (l `exclude` (Impl x y)) (x:r)) (check (y : (l `exclude` (Impl x y))) r) 

rfSwapper :: Exp -> [Exp] -> [Exp] -> Tree
rfSwapper (Not x) l r   = LonelyBranch "|r not|" (check (x:l) (r `exclude` (Not x))) 
rfSwapper (Or x y) l r = LonelyBranch "|r or|" (check l (x : y : (r `exclude` (Or x y)))) 
rfSwapper (Impl x y) l r = LonelyBranch "|r impl|" (check (x:l) (y : (r `exclude` (Impl x y))))
rfSwapper (And x y) l r = Branch "|r and|" (check l (x : (r `exclude` (And x y)))) (check l (y : (r `exclude` (And x y)))) 

exclude :: [Exp] -> Exp -> [Exp]
exclude l e = filter (\x -> x /= e) l

getResult :: Tree -> Bool
getResult (Leaf s) = if s == "TQBF" then True else False
getResult (LonelyBranch s t) = getResult t
getResult (Branch s l r) = getResult l && getResult r

-- tests 
wikiTest = ForAll "x" $ Exists "y" $ Exists "z" $ And (Or (Var "x") (Var "z")) (Var "y") -- ok

t1 = Exists "x" $ ForAll "y" $ And (Or (Var "x") (Not $ Var "y")) (Or (Not $ Var "x") (Var "y"))
t2 = ForAll "y" $ Exists "x" $ And (Or (Var "x") (Not $ Var "y")) (Or (Not $ Var "x") (Var "y"))
t3 = Or (Var "x") (Not (Var "x")) -- ok